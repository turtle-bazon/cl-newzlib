(in-package #:cl-newzlib)

;;; Stateful streaming DEFLATE compressor (RFC 1950/1951/1952).
;;;
;;; Unlike DEFLATE-STREAM (which buffers its whole input and compresses it in
;;; one pass on finish), a DEFLATE-STATE keeps the LZ77 window, hash chains,
;;; Huffman coding state and checksum alive across calls.  Each DEFLATE-STATE-
;;; WRITE tokenizes newly supplied input against the retained 32 KiB history
;;; and emits a non-final block; DEFLATE-STATE-FLUSH emits an empty block so
;;; the peer can decompress everything written so far without ending the
;;; stream.  This is the shape SSH's per-packet compression (RFC 4253 6.2)
;;; needs, and it interoperates with zlib's Z_PARTIAL_FLUSH / Z_SYNC_FLUSH.
;;;
;;; The input history lives in a sliding buffer.  When the write position
;;; crosses two windows the buffer is compacted by a whole number of windows
;;; (required so the PREV ring indexes stay aligned) and every hash-chain
;;; position is rebased, dropping entries that fell out of the window.  Memory
;;; is therefore O(window + pending output), independent of stream length.

(defconstant +stream-data-size+ 131072
  "Initial sliding-window buffer size (four 32 KiB windows).")

(defconstant +stream-chunk-size+ 32768
  "Maximum number of input octets tokenized before a block is emitted.")

(defconstant +stream-compact-threshold+ 65536
  "Compact the sliding window once its write position reaches this.")

(defstruct (deflate-state
            (:constructor %make-deflate-state)
            (:conc-name dst-)
            (:predicate deflate-state-p)
            (:copier nil))
  (format :zlib :type keyword)
  (level +default-compression+ :type fixnum)
  (strategy :default :type keyword)
  (window-bits 15 :type fixnum)
  (mem-level 8 :type fixnum)
  (mode :standard :type keyword)
  (scratch nil :type (or null lz77-scratch))
  (writer nil :type (or null bit-writer))
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (data-pos 0 :type fixnum)
  (checksum 1 :type (unsigned-byte 32))
  (total-in 0 :type integer)
  (finished nil :type boolean)
  (ended nil :type boolean))

(defun check-stream-format (format)
  (unless (member format '(:zlib :gzip :raw))
    (error 'newzlib-parameter-error
           :detail (format nil "unsupported format ~S (expected :zlib, :gzip or :raw)"
                           format)))
  format)

(defun check-deflate-strategy (strategy)
  (unless (member strategy '(:default :huffman-only :fixed :rle))
    (error 'newzlib-parameter-error
           :detail (format nil "unsupported compression strategy ~S ~
(expected :default, :huffman-only, :fixed or :rle)"
                           strategy)))
  strategy)

(defun check-octet-arguments (octets start end)
  "Validate an OCTETS/START/END argument triple and return the effective END."
  (unless (typep octets '(simple-array (unsigned-byte 8) (*)))
    (error 'newzlib-parameter-error
           :detail "input must be an (unsigned-byte 8) vector"))
  (unless (and (integerp start) (or (null end) (integerp end)))
    (error 'newzlib-parameter-error :detail "start/end must be integers"))
  (let ((end (or end (length octets))))
    (unless (<= 0 start end (length octets))
      (error 'newzlib-parameter-error :detail "invalid start/end range"))
    end))

(defun check-window-bits (window-bits)
  (unless (and (integerp window-bits) (<= 8 window-bits 15))
    (error 'newzlib-parameter-error
           :detail (format nil "window-bits ~A must be an integer in [8, 15]"
                           window-bits)))
  window-bits)

(defun check-mem-level (mem-level)
  (unless (and (integerp mem-level) (<= 1 mem-level 9))
    (error 'newzlib-parameter-error
           :detail (format nil "mem-level ~A must be an integer in [1, 9]"
                           mem-level)))
  mem-level)

(defun dst-emit-header (state)
  "Commit the wrapper header for a fresh stream to the output writer."
  (let ((writer (dst-writer state)))
    (case (dst-format state)
      (:zlib
       (let ((header (zlib-header-octets (dst-level state))))
         (dotimes (i (length header))
           (write-bits writer (aref header i) 8))))
      (:gzip
       (let ((header (gzip-header-octets (dst-level state))))
         (dotimes (i (length header))
           (write-bits writer (aref header i) 8))))
      (t nil))
    (flush-pending-bytes writer))
  nil)

(defun dst-empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun dst-drain (state)
  "Return every complete octet produced since the last drain.  Bits that do
not yet form a whole octet stay in the writer, so no stream data is lost."
  (let* ((writer (dst-writer state))
         (n (progn (flush-pending-bytes writer) (bw-pos writer)))
         (out (make-octet-buffer n)))
    (when (plusp n)
      (replace out (bw-buffer writer) :end2 n)
      (setf (bw-pos writer) 0))
    out))

(defun dst-append-octets (writer octets)
  "Append OCTETS to WRITER at a byte boundary.  Caller must have flushed the
pending bits (DEFLATE-STATE-FINISH does this before writing the trailer)."
  (let* ((n (length octets))
         (pos (bw-pos writer))
         (size (bw-size writer)))
    (when (> (+ pos n) size)
      (let ((new-size (max (* 2 size) (+ pos n))))
        (let ((new (make-octet-buffer new-size)))
          (replace new (bw-buffer writer) :end2 size)
          (setf (bw-buffer writer) new
                (bw-size writer) new-size
                size new-size))))
    (replace (bw-buffer writer) octets :start1 pos :end2 n)
    (setf (bw-pos writer) (+ pos n)))
  nil)

(defun dst-emit-empty-fixed (state bfinal)
  "Emit an empty fixed-Huffman block.  A partial flush is BFINAL=0 followed
by this 10-bit block, matching zlib's Z_PARTIAL_FLUSH."
  (let ((writer (dst-writer state)))
    (write-bits writer (if bfinal 1 0) 1)
    (write-bits writer 1 2)             ; BTYPE = 01, fixed Huffman
    (write-bits writer 0 7)             ; end-of-block symbol 256
    (flush-pending-bytes writer))
  nil)

(defun dst-emit-empty-stored (state bfinal)
  "Emit an empty stored block.  A sync flush is BFINAL=0 followed by this,
which byte-aligns the output (`00 00 FF FF`, RFC 1951 3.2.4)."
  (let ((writer (dst-writer state)))
    (write-bits writer (if bfinal 1 0) 1)
    (write-bits writer 0 2)             ; BTYPE = 00, stored
    (align-writer writer)
    (write-bits writer 0 16)
    (write-bits writer #xFFFF 16)
    (flush-pending-bytes writer))
  nil)

(defun dst-compact-data (state)
  "Rebase the sliding window by a whole number of 32 KiB windows, retaining
at least one full window of history.  SHIFT must be a multiple of
+WINDOW-SIZE+ so the PREV ring indexes remain aligned."
  (let ((pos (dst-data-pos state)))
    (when (>= pos +stream-compact-threshold+)
      (let* ((shift (- (* +window-size+ (floor pos +window-size+))
                       +window-size+))
             (data (dst-data state))
             (scratch (dst-scratch state))
             (head (lzs-head scratch))
             (prev (lzs-prev scratch)))
        (declare (type fixnum shift))
        (replace data data :start1 0 :start2 shift :end2 pos)
        (setf (dst-data-pos state) (- pos shift))
        (dotimes (i +hash-size+)
          (let ((v (aref head i)))
            (setf (aref head i)
                  (if (and (/= v #xFFFFFFFF) (>= v shift))
                      (- v shift)
                      #xFFFFFFFF))))
        (dotimes (i +window-size+)
          (let ((v (aref prev i)))
            (setf (aref prev i)
                  (if (and (/= v #xFFFFFFFF) (>= v shift))
                      (- v shift)
                      #xFFFFFFFF)))))))
  nil)

(defun dst-append-data (state octets start end)
  "Append OCTETS[START,END) to the sliding window at its write position."
  (let* ((n (- end start))
         (pos (dst-data-pos state))
         (need (+ pos n))
         (data (dst-data state)))
    (declare (type fixnum n pos need))
    (when (> need (length data))
      (let ((new (make-octet-buffer (max (* 2 (length data)) need))))
        (replace new data :end2 pos)
        (setf data new
              (dst-data state) new)))
    (replace data octets :start1 pos :start2 start :end2 end)
    (setf (dst-data-pos state) need))
  nil)

(defun dst-reset-internal (state)
  "Discard every trace of the current stream and start a new one, emitting
its wrapper header (if any) into the writer."
  (let ((scratch (dst-scratch state)))
    (fill (lzs-head scratch) #xFFFFFFFF)
    (fill (lzs-prev scratch) #xFFFFFFFF)
    (fill (lzs-lit-freq scratch) 0)
    (fill (lzs-dist-freq scratch) 0)
    (fill (lzs-bl-freq scratch) 0))
  (setf (dst-data-pos state) 0
        (dst-checksum state) (if (eq (dst-format state) :zlib) 1 0)
        (dst-total-in state) 0
        (dst-finished state) nil)
  (reset-bit-writer (dst-writer state) 4096)
  (dst-emit-header state)
  nil)

(defun deflate-state-p-internal-check (state)
  (unless (deflate-state-p state)
    (error 'newzlib-parameter-error :detail "not a deflate-state"))
  (when (dst-ended state)
    (error 'newzlib-parameter-error :detail "operation on an ended deflate-state"))
  state)

(defun make-deflate-state (&key (format :zlib)
                                (level +default-compression+)
                                (strategy :default)
                                (window-bits 15)
                                (mem-level 8)
                                (mode :standard))
  "Create a stateful DEFLATE compressor.  FORMAT is :ZLIB (default), :GZIP
or :RAW.  LEVEL is 0-9.  STRATEGY is :DEFAULT, :HUFFMAN-ONLY (literal-only,
matching OpenSSH's current outgoing strategy), :FIXED (fixed Huffman blocks)
or :RLE (distance-1 runs).  MODE :FAST selects greedy LZ77 matching.

The compressor always uses the full 32 KiB DEFLATE window; WINDOW-BITS is
accepted for API compatibility with zlib's deflateInit2 and must be 8-15.
Feed input with DEFLATE-STATE-WRITE and end each message boundary with
DEFLATE-STATE-FLUSH.  The state is not thread-safe: use one object per
direction."
  (check-stream-format format)
  (check-compression-level level)
  (check-deflate-strategy strategy)
  (check-window-bits window-bits)
  (check-mem-level mem-level)
  (check-compression-mode mode)
  (let* ((scratch (acquire-lz77-scratch 4096))
         (state (%make-deflate-state
                 :format format :level level :strategy strategy
                 :window-bits window-bits :mem-level mem-level :mode mode
                 :scratch scratch
                 :writer (lzs-writer scratch)
                 :data (make-octet-buffer +stream-data-size+))))
    (dst-reset-internal state)
    state))

(defun deflate-state-write (state octets &key (start 0) (end nil))
  "Feed OCTETS[START,END) to STATE and return any compressed octets that are
now complete.  Input is tokenized against the retained window and emitted as
one non-final block per internal chunk (at most 32 KiB); callers must not
assume that all input is represented until FLUSH is called."
  (deflate-state-p-internal-check state)
  (let ((end (check-octet-arguments octets start end)))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets)
             (type fixnum start end))
    (when (dst-finished state)
      (error 'newzlib-parameter-error
             :detail "write after deflate-state-finish"))
    (unless (= start end)
      (let ((n (- end start)))
        (case (dst-format state)
          (:zlib
           (setf (dst-checksum state)
                 (adler32 octets start end (dst-checksum state))))
          (:gzip
           (setf (dst-checksum state)
                 (crc32-update (dst-checksum state) octets start end)))
          (t nil))
        (incf (dst-total-in state) n)
        (loop for s fixnum = start then (+ s +stream-chunk-size+)
              while (< s end)
              do (let ((e (min end (+ s +stream-chunk-size+))))
                   (dst-compact-data state)
                   (let ((from (dst-data-pos state)))
                     (dst-append-data state octets s e)
                     (deflate-block-into-writer
                      (dst-data state) from (dst-data-pos state)
                      (dst-writer state) (dst-level state)
                      (dst-scratch state) (dst-mode state)
                      (dst-strategy state) nil))))))
    (dst-drain state)))

(defun deflate-state-flush (state &key (mode :partial))
  "Finish the current block and return all octets produced so far without
ending the stream.  MODE :PARTIAL (the default) emits zlib's 10-bit
Z_PARTIAL_FLUSH marker; :SYNC emits an empty stored block, byte-aligning the
output.  Both are accepted by a peer running inflate with Z_SYNC_FLUSH."
  (deflate-state-p-internal-check state)
  (when (dst-finished state)
    (error 'newzlib-parameter-error
           :detail "flush after deflate-state-finish"))
  (case mode
    (:partial (dst-emit-empty-fixed state nil))
    (:sync (dst-emit-empty-stored state nil))
    (t (error 'newzlib-parameter-error
              :detail (format nil "unsupported flush mode ~S (expected :partial or :sync)"
                              mode))))
  (dst-drain state))

(defun deflate-state-finish (state)
  "End the stream: emit a final empty block, pad to a byte boundary, write the
format trailer (:zlib Adler-32 or :gzip CRC-32/ISIZE) and return the remaining
compressed octets.  After this, only RESET and END are valid.  Calling FINISH
twice returns an empty vector the second time."
  (deflate-state-p-internal-check state)
  (cond
    ((dst-finished state) (dst-empty-octets))
    (t
     (let ((writer (dst-writer state)))
       (dst-emit-empty-fixed state t)
       (flush-bits writer)
       (case (dst-format state)
         (:zlib
          (let ((checksum (dst-checksum state))
                (trailer (make-octet-buffer 4)))
            (setf (aref trailer 0) (ldb (byte 8 24) checksum)
                  (aref trailer 1) (ldb (byte 8 16) checksum)
                  (aref trailer 2) (ldb (byte 8 8) checksum)
                  (aref trailer 3) (ldb (byte 8 0) checksum))
            (dst-append-octets writer trailer)))
         (:gzip
          (let ((crc (dst-checksum state))
                (size (ldb (byte 32 0) (dst-total-in state)))
                (trailer (make-octet-buffer 8)))
            (setf (aref trailer 0) (ldb (byte 8 0) crc)
                  (aref trailer 1) (ldb (byte 8 8) crc)
                  (aref trailer 2) (ldb (byte 8 16) crc)
                  (aref trailer 3) (ldb (byte 8 24) crc)
                  (aref trailer 4) (ldb (byte 8 0) size)
                  (aref trailer 5) (ldb (byte 8 8) size)
                  (aref trailer 6) (ldb (byte 8 16) size)
                  (aref trailer 7) (ldb (byte 8 24) size))
            (dst-append-octets writer trailer)))
         (t nil)))
     (setf (dst-finished state) t)
     (dst-drain state))))

(defun deflate-state-reset (state)
  "Discard the window, hash chains, Huffman state and checksum and begin a new
stream for the same direction (SSH calls this at NEWKEYS).  Returns the new
stream's header octets: two for :ZLIB, ten for :GZIP, none for :RAW.  Any
unflushed input is dropped."
  (deflate-state-p-internal-check state)
  (dst-reset-internal state)
  (dst-drain state))

(defun deflate-state-end (state)
  "Release the resources held by STATE.  Safe to call more than once."
  (when (deflate-state-p state)
    (unless (dst-ended state)
      (when (dst-scratch state)
        (release-lz77-scratch (dst-scratch state)))
      (setf (dst-scratch state) nil
            (dst-writer state) nil
            (dst-data state) nil
            (dst-ended state) t)))
  nil)

(defun join-octets (&rest vectors)
  (let* ((n (reduce #'+ vectors :key #'length :initial-value 0))
         (out (make-octet-buffer n))
         (pos 0))
    (declare (type fixnum n pos))
    (dolist (v vectors out)
      (let ((m (length v)))
        (replace out v :start1 pos :end2 m)
        (incf pos m)))))

(defun deflate-packet (state octets &key (start 0) (end nil) (mode :partial))
  "Compress one message (OCTETS[START,END)) and flush it, returning the
compressed octets.  Equivalent to DEFLATE-STATE-WRITE followed by
DEFLATE-STATE-FLUSH."
  (join-octets (deflate-state-write state octets :start start :end end)
               (deflate-state-flush state :mode mode)))
