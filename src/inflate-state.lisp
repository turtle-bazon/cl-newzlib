(in-package #:cl-newzlib)

;;; Stateful streaming DEFLATE decompressor (RFC 1950/1951/1952).
;;;
;;; INFLATE-STREAM decompresses its entire input on the first read.  An
;;; INFLATE-STATE instead keeps the DEFLATE window, block boundaries and
;;; wrapper checksum across calls, so a peer can feed one flushed packet at a
;;; time without ever presenting a complete stream.
;;;
;;; Input is carried in a byte buffer and consumed through a resumable bit
;;; reader.  Compressed input that does not yet contain a whole block is kept
;;; (only the current block, never the whole session) and retried when more
;;; octets arrive; complete blocks are decoded immediately.  Decoded output is
;;; retained as a sliding history window so later matches can refer back to it.

(defstruct (inflate-state
            (:constructor %make-inflate-state)
            (:conc-name ist-)
            (:predicate inflate-state-p)
            (:copier nil))
  (format :zlib :type keyword)
  (window-bits 15 :type fixnum)
  (in-buf nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (in-size 0 :type fixnum)
  (in-end 0 :type fixnum)
  (data-start 0 :type fixnum)
  (reader nil :type (or null bit-reader))
  (header-done nil :type boolean)
  (out-buf nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (out-size 0 :type fixnum)
  (out-pos 0 :type fixnum)
  (checksum 1 :type (unsigned-byte 32))
  (total-out 0 :type integer)
  (awaiting-trailer nil :type boolean)
  (finished nil :type boolean)
  (ended nil :type boolean)
  (scratch nil :type (or null inflate-scratch)))

(defun ist-empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun ist-internal-check (state)
  (unless (inflate-state-p state)
    (error 'newzlib-parameter-error :detail "not an inflate-state"))
  (when (ist-ended state)
    (error 'newzlib-parameter-error :detail "operation on an ended inflate-state"))
  state)

(defun ist-append-input (state octets start end)
  "Append OCTETS[START,END) to the compressed-input buffer."
  (let* ((n (- end start))
         (old-end (ist-in-end state))
         (need (+ old-end n))
         (buf (ist-in-buf state)))
    (declare (type fixnum n old-end need))
    (when (> need (ist-in-size state))
      (let ((new-size (max 4096 (* 2 (ist-in-size state)) need)))
        (declare (type fixnum new-size))
        (let ((new (make-octet-buffer new-size)))
          (replace new buf :end2 old-end)
          (setf buf new
                (ist-in-buf state) new
                (ist-in-size state) new-size))))
    (replace buf octets :start1 old-end :start2 start :end2 end)
    (setf (ist-in-end state) need)
    (let ((reader (ist-reader state)))
      (when reader
        (setf (br-buffer reader) buf
              (br-end reader) need))))
  nil)

;;; ------------------------------------------------------------------
;;; Wrapper headers
;;; ------------------------------------------------------------------

(defun ist-parse-zlib-header (state)
  "Validate the RFC 1950 header when its two octets are available.  Returns
true when the header has been consumed, false when more input is needed."
  (let* ((buf (ist-in-buf state))
         (end (ist-in-end state)))
    (when (< end 2)
      (return-from ist-parse-zlib-header nil))
    (let ((cmf (aref buf 0))
          (flg (aref buf 1)))
      (unless (= (logand cmf #x0F) 8)
        (error 'newzlib-format-error
               :detail "unsupported zlib compression method"))
      (unless (<= (logand (ash cmf -4) #x0F) 7)
        (error 'newzlib-format-error
               :detail "invalid zlib window size"))
      (unless (zerop (mod (+ (* cmf 256) flg) 31))
        (error 'newzlib-format-error
               :detail "zlib header checksum mismatch"))
      (when (logbitp 5 flg)
        (error 'newzlib-unsupported-error
               :detail "zlib preset dictionaries are not supported"))
      (setf (ist-window-bits state) (+ 8 (logand (ash cmf -4) #x0F))
            (ist-data-start state) 2
            (ist-header-done state) t))
    t))

(defun ist-parse-gzip-header (state)
  "Validate the RFC 1952 header when all of it is available.  Returns true
when consumed, false when more input is needed."
  (let* ((buf (ist-in-buf state))
         (end (ist-in-end state)))
    (when (< end 10)
      (return-from ist-parse-gzip-header nil))
    (unless (and (= (aref buf 0) #x1F) (= (aref buf 1) #x8B))
      (error 'newzlib-format-error :detail "invalid gzip magic"))
    (unless (= (aref buf 2) 8)
      (error 'newzlib-format-error
             :detail "unsupported gzip compression method"))
    (let ((flg (aref buf 3))
          (pos 10))
      (declare (type fixnum pos))
      (when (logbitp 2 flg)             ; FEXTRA
        (when (> (+ pos 2) end)
          (return-from ist-parse-gzip-header nil))
        (let ((xlen (+ (aref buf pos) (ash (aref buf (1+ pos)) 8))))
          (incf pos (+ 2 xlen))
          (when (> pos end)
            (return-from ist-parse-gzip-header nil))))
      (when (logbitp 3 flg)             ; FNAME
        (let ((nul (position 0 buf :start pos :end end)))
          (if nul
              (setf pos (1+ nul))
              (return-from ist-parse-gzip-header nil))))
      (when (logbitp 4 flg)             ; FCOMMENT
        (let ((nul (position 0 buf :start pos :end end)))
          (if nul
              (setf pos (1+ nul))
              (return-from ist-parse-gzip-header nil))))
      (when (logbitp 1 flg)             ; FHCRC
        (when (> (+ pos 2) end)
          (return-from ist-parse-gzip-header nil))
        (incf pos 2))
      (setf (ist-window-bits state) 15
            (ist-data-start state) pos
            (ist-header-done state) t))
    t))

(defun ist-parse-header (state)
  (case (ist-format state)
    (:raw
     (setf (ist-data-start state) 0
           (ist-header-done state) t)
     t)
    (:zlib (ist-parse-zlib-header state))
    (:gzip (ist-parse-gzip-header state))))

(defun ist-make-reader (state)
  (unless (ist-reader state)
    (setf (ist-reader state)
          (make-bit-reader (ist-in-buf state)
                           (ist-data-start state)
                           (ist-in-end state))))
  nil)

;;; ------------------------------------------------------------------
;;; Output history and checksum
;;; ------------------------------------------------------------------

(defun ist-account-output (state from)
  "Add OUT-BUF[FROM,OUT-POS) to the wrapper checksum and size counters."
  (let ((to (ist-out-pos state))
        (buf (ist-out-buf state)))
    (declare (type fixnum from to))
    (when (> to from)
      (case (ist-format state)
        (:zlib
         (setf (ist-checksum state)
               (adler32 buf from to (ist-checksum state))))
        (:gzip
         (setf (ist-checksum state)
               (crc32-update (ist-checksum state) buf from to)))
        (t nil))
      (incf (ist-total-out state) (- to from))))
  nil)

(defun ist-compact-output (state)
  "Keep at most one full DEFLATE window of decoded history."
  (let ((pos (ist-out-pos state)))
    (when (> pos (* 2 +window-size+))
      (let* ((shift (- pos +window-size+))
             (buf (ist-out-buf state)))
        (declare (type fixnum shift))
        (replace buf buf :start1 0 :start2 shift :end2 pos)
        (setf (ist-out-pos state) +window-size+))))
  nil)

(defun ist-compact-input (state)
  "Drop compressed octets already absorbed into the reader accumulator."
  (let ((reader (ist-reader state)))
    (when reader
      (let ((pos (br-pos reader))
            (end (br-end reader))
            (buf (ist-in-buf state)))
        (declare (type fixnum pos end))
        (when (plusp pos)
          (replace buf buf :start1 0 :start2 pos :end2 end)
          (setf (br-pos reader) 0
                (br-end reader) (- end pos)
                (ist-in-end state) (- end pos))))))
  nil)

;;; ------------------------------------------------------------------
;;; Block decoding
;;; ------------------------------------------------------------------

(defun ist-decode-block (state)
  "Decode one DEFLATE block into the output history.  Returns BFINAL.  On
truncated input NEWZLIB-END-OF-INPUT escapes before any state is committed;
the caller restores the reader and output position."
  (let* ((reader (ist-reader state))
         (buffer (ist-out-buf state))
         (size (ist-out-size state))
         (pos (ist-out-pos state)))
    (let ((bfinal (read-bits reader 1))
          (btype (read-bits reader 2)))
      (declare (type fixnum bfinal btype))
      (multiple-value-bind (nbuffer npos nsize)
          (case btype
            (0 (inflate-stored-block reader buffer size pos nil))
            (1 (multiple-value-bind (lit dist) (ensure-fixed-tables)
                 (%inflate-token-stream reader buffer size pos lit dist nil)))
            (2 (multiple-value-bind (lit dist)
                   (inflate-dynamic-header reader (ist-scratch state))
                 (%inflate-token-stream reader buffer size pos lit dist nil)))
            (otherwise
             (error 'newzlib-format-error :detail "invalid block type")))
        (setf (ist-out-buf state) nbuffer
              (ist-out-size state) nsize
              (ist-out-pos state) npos))
      bfinal)))

(defun ist-read-trailer (state)
  "Consume and validate the wrapper trailer if all of it is available.
Returns true when the stream has finished."
  (let ((reader (ist-reader state)))
    (let ((accum (br-accum reader))
          (nbits (br-nbits reader))
          (rpos (br-pos reader)))
      (handler-case
          (progn
            (align-reader reader)
            (case (ist-format state)
              (:zlib
               (let* ((b0 (read-bits reader 8))
                      (b1 (read-bits reader 8))
                      (b2 (read-bits reader 8))
                      (b3 (read-bits reader 8))
                      (expected (logior (ash b0 24) (ash b1 16)
                                        (ash b2 8) b3)))
                 (unless (= expected (logand (ist-checksum state) #xFFFFFFFF))
                   (error 'newzlib-format-error
                          :detail "zlib Adler-32 mismatch"))))
              (:gzip
               (let* ((c0 (read-bits reader 8))
                      (c1 (read-bits reader 8))
                      (c2 (read-bits reader 8))
                      (c3 (read-bits reader 8))
                      (s0 (read-bits reader 8))
                      (s1 (read-bits reader 8))
                      (s2 (read-bits reader 8))
                      (s3 (read-bits reader 8))
                      (crc (logior c0 (ash c1 8) (ash c2 16) (ash c3 24)))
                      (size (logior s0 (ash s1 8) (ash s2 16) (ash s3 24))))
                 (unless (= crc (logand (ist-checksum state) #xFFFFFFFF))
                   (error 'newzlib-format-error
                          :detail "gzip CRC-32 mismatch"))
                 (unless (= size (logand (ist-total-out state) #xFFFFFFFF))
                   (error 'newzlib-format-error
                          :detail "gzip ISIZE mismatch"))))
              (t nil))
            (setf (ist-finished state) t
                  (ist-awaiting-trailer state) nil)
            t)
        (newzlib-end-of-input ()
          (setf (br-accum reader) accum
                (br-nbits reader) nbits
                (br-pos reader) rpos)
          nil)))))

;;; ------------------------------------------------------------------
;;; Public API
;;; ------------------------------------------------------------------

(defun make-inflate-state (&key (format :zlib) (window-bits 15))
  "Create a stateful DEFLATE decompressor.  FORMAT is :ZLIB (default), :GZIP
or :RAW.  Feed compressed octets with INFLATE-STATE-WRITE and read the
plaintext it returns.  The decoder keeps only the current incomplete block and
one 32 KiB output window, so memory does not grow with the session length.
WINDOW-BITS is accepted for zlib API compatibility and must be 8-15; the
decoder always retains a full 32 KiB history, so a smaller setting never
rejects a valid stream.  zlib and gzip headers carry their own window size.
The state is not thread-safe."
  (check-stream-format format)
  (check-window-bits window-bits)
  (%make-inflate-state
   :format format
   :window-bits window-bits
   :checksum (if (eq format :zlib) 1 0)
   :in-buf (make-octet-buffer 4096)
   :in-size 4096
   :out-buf (make-octet-buffer +inflate-start-size+)
   :out-size +inflate-start-size+
   :scratch (acquire-inflate-scratch)))

(defun inflate-state-write (state octets &key (start 0) (end nil))
  "Feed OCTETS[START,END) to STATE and return all plaintext that can be
produced from complete blocks now.  Truncated input is not an error: the
current block is retained and retried when the next call supplies more
octets.  Corrupt DEFLATE or a bad wrapper checksum signals
NEWZLIB-FORMAT-ERROR."
  (ist-internal-check state)
  (let ((end (check-octet-arguments octets start end)))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets)
             (type fixnum start end))
    (when (ist-finished state)
      (if (< start end)
          (error 'newzlib-format-error
                 :detail "compressed data after end of stream")
          (return-from inflate-state-write (ist-empty-octets))))
    (ist-append-input state octets start end)
    (unless (ist-header-done state)
      (ist-parse-header state))
    (when (ist-header-done state)
      (ist-make-reader state))
    (let ((call-start (ist-out-pos state)))
      (when (ist-header-done state)
        (let ((continue t))
          ;; Decode complete blocks.  A truncated tail leaves the reader and
          ;; the output position exactly where the last block ended.
          (loop while (and continue
                           (not (ist-finished state))
                           (not (ist-awaiting-trailer state)))
                do (let ((reader (ist-reader state))
                         (accum (br-accum (ist-reader state)))
                         (nbits (br-nbits (ist-reader state)))
                         (rpos (br-pos (ist-reader state)))
                         (before (ist-out-pos state)))
                     (handler-case
                         (let ((bfinal (ist-decode-block state)))
                           (ist-account-output state before)
                           (cond
                             ((and (plusp bfinal)
                                   (eq (ist-format state) :raw))
                              (setf (ist-finished state) t))
                             ((plusp bfinal)
                              (setf (ist-awaiting-trailer state) t))))
                       (newzlib-end-of-input ()
                         (setf (br-accum reader) accum
                               (br-nbits reader) nbits
                               (br-pos reader) rpos)
                         (setf (ist-out-pos state) before
                               continue nil)))))
          (when (and (ist-awaiting-trailer state)
                     (not (ist-finished state)))
            (ist-read-trailer state))))
      (let* ((stop (ist-out-pos state))
             (n (- stop call-start))
             (result (make-octet-buffer n))
             (buf (ist-out-buf state)))
        (declare (type fixnum stop n))
        (when (plusp n)
          (replace result buf :start2 call-start :end2 stop))
        (ist-compact-output state)
        (ist-compact-input state)
        result))))

(defun inflate-state-flush (state)
  "Return any plaintext still held internally.  INFLATE-STATE-WRITE already
returns every byte of each complete block, so this normally returns an empty
vector; it exists for symmetry with DEFLATE-STATE-FLUSH."
  (ist-internal-check state)
  (ist-empty-octets))

(defun inflate-state-finished-p (state)
  "True once the final block and the format trailer (if any) have been seen
and verified."
  (ist-internal-check state)
  (ist-finished state))

(defun inflate-state-reset (state)
  "Discard all decoder state for the next key exchange: buffered input,
output history, checksum and block state.  Returns NIL."
  (ist-internal-check state)
  (setf (ist-in-end state) 0
        (ist-data-start state) 0
        (ist-reader state) nil
        (ist-header-done state) nil
        (ist-out-pos state) 0
        (ist-checksum state) (if (eq (ist-format state) :zlib) 1 0)
        (ist-total-out state) 0
        (ist-awaiting-trailer state) nil
        (ist-finished state) nil)
  nil)

(defun inflate-state-end (state)
  "Release the resources held by STATE.  Safe to call more than once."
  (when (inflate-state-p state)
    (unless (ist-ended state)
      (when (ist-scratch state)
        (release-inflate-scratch (ist-scratch state)))
      (setf (ist-scratch state) nil
            (ist-in-buf state) nil
            (ist-out-buf state) nil
            (ist-reader state) nil
            (ist-ended state) t)))
  nil)

(defun inflate-packet (state octets &key (start 0) (end nil))
  "Decompress one flushed message (OCTETS[START,END)) and return its
plaintext.  Equivalent to INFLATE-STATE-WRITE followed by INFLATE-STATE-FLUSH."
  (join-octets (inflate-state-write state octets :start start :end end)
               (inflate-state-flush state)))
