(in-package #:cl-newzlib)

;;; DEFLATE decompressor (RFC 1951).
;;;
;;; Reads a raw DEFLATE stream from a bit-reader, handling stored, fixed and
;;; dynamic blocks, and appends decoded output to a growable octet buffer.
;;; Output is a plain (unsigned-byte 8) array with an explicit position;
;;; matches are copied in chunks so long runs avoid per-byte pushes.

;;; ------------------------------------------------------------------
;;; Fixed Huffman trees
;;; ------------------------------------------------------------------

(defparameter +fixed-lit-lengths+
  (let ((v (make-array 288 :element-type 'fixnum :initial-element 0)))
    (loop for n from 0 below 144 do (setf (aref v n) 8))
    (loop for n from 144 below 256 do (setf (aref v n) 9))
    (loop for n from 256 below 280 do (setf (aref v n) 7))
    (loop for n from 280 below 288 do (setf (aref v n) 8))
    v))

(defparameter +fixed-dist-lengths+
  (make-array 30 :element-type 'fixnum :initial-element 5))

;;; The fixed decode tables are built eagerly at load time.  A previous
;;; lazy check-then-act initialization was a data race: the two SETFs are
;;; not atomic together, so a thread entering between them observed a built
;;; literal table alongside a NIL distance table and -- with the hot paths
;;; compiled at safety 0 -- faulted on the NIL.  Eager construction makes
;;; the race impossible by construction.
(defparameter +fixed-lit-table+
  (build-huffman-decode-table +fixed-lit-lengths+))
(defparameter +fixed-dist-table+
  (build-huffman-decode-table +fixed-dist-lengths+))

(declaim (type huffman-decode-table +fixed-lit-table+ +fixed-dist-table+))

(defun ensure-fixed-tables ()
  "Return the fixed Huffman decode tables (built at load time)."
  (values +fixed-lit-table+ +fixed-dist-table+))

;;; ------------------------------------------------------------------
;;; Bit alignment
;;; ------------------------------------------------------------------

(declaim (inline align-reader))
(defun align-reader (reader)
  "Consume bits up to the next byte boundary of READER."
  (declare (optimize (speed 3) (safety 0)))
  (let ((n (logand 7 (br-nbits reader))))
    (when (plusp n)
      (read-bits reader n)))
  nil)

;;; ------------------------------------------------------------------
;;; Output buffer
;;; ------------------------------------------------------------------

(declaim (inline ensure-out-capacity))
(defun ensure-out-capacity (buffer size pos need)
  "Grow BUFFER so that at least NEED bytes fit starting at POS.  Returns
(VALUES BUFFER NEW-SIZE)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size pos need))
  (if (<= (+ pos need) size)
      (values buffer size)
      ;; grow 4x at a time: repeated doubling spends ~1x of the final size
      ;; in copied bytes, 4x growth spends ~1/3x
      (let ((new-size size))
        (loop while (< new-size (+ pos need)) do (setf new-size (* 4 new-size)))
        (let ((new (make-octet-buffer new-size)))
          (replace new buffer :end2 size)
          (values new new-size)))))

;;; ------------------------------------------------------------------
;;; Stored blocks
;;; ------------------------------------------------------------------

(defun inflate-stored-block (reader buffer size pos)
  "Decode one stored block into BUFFER[POS..].  Returns (VALUES BUFFER POS SIZE)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size pos))
  (align-reader reader)
  (let ((len (read-bits reader 16))
        (nlen (read-bits reader 16)))
    (declare (type fixnum len))
    (unless (= (logand (lognot len) #xFFFF) nlen)
      (error 'newzlib-format-error :detail "stored block length mismatch"))
    (multiple-value-bind (buffer size)
        (ensure-out-capacity buffer size pos len)
      ;; drain whole bytes still pending in the reader accumulator
      (iterate:iterate
        (iterate:while (and (plusp len) (>= (br-nbits reader) 8)))
        (setf (aref buffer pos) (read-bits reader 8))
        (incf pos)
        (decf len))
      ;; the rest is byte-aligned in the input buffer
      (when (plusp len)
        (let ((n (min len (- (br-end reader) (br-pos reader)))))
          (replace buffer (br-buffer reader)
                   :start1 pos :start2 (br-pos reader)
                   :end1 (+ pos n) :end2 (+ (br-pos reader) n))
          (setf (br-pos reader) (+ (br-pos reader) n)
                pos (+ pos n)
                len (- len n)))
        (iterate:iterate
          (iterate:while (plusp len))
          (setf (aref buffer pos) (read-bits reader 8))
          (incf pos)
          (decf len)))
      (values buffer pos size))))

;;; ------------------------------------------------------------------
;;; Token stream (fixed and dynamic blocks)
;;; ------------------------------------------------------------------

;;; Private bit primitives over INFLATE-TOKEN-STREAM's loop locals
;;; (ACCUM NBITS RPOS REND RBUF RSAP READER, the LIT/DIST table locals,
;;; SYM DSYM and the output BUFFER SIZE POS).  Top-level (rather than
;;; MACROLET) so the main loop fits the function-size rule; expansions
;;; are unchanged.
;;; ------------------------------------------------------------------

(defmacro %inflate-refill (need)
  "Top ACCUM up to at least NEED bits; signals NEWZLIB-END-OF-INPUT first."
  `(loop while (< nbits ,need) do
     (when (>= rpos rend)
       (setf (br-accum reader) accum
             (br-nbits reader) nbits
             (br-pos reader) rpos)
       (error 'newzlib-end-of-input))
     #+(and sbcl cl-newzlib-le)
     (loop while (and (<= nbits 24) (<= (+ rpos 4) rend)) do
       (setf accum (logior accum
                           (definitely-the (unsigned-byte 64)
                             (ash (%word-at rsap rpos) nbits)))
             nbits (+ nbits 32)
             rpos (+ rpos 4)))
     (when (< rpos rend)
       (setf accum (logior accum
                           (definitely-the (unsigned-byte 64)
                             (ash (aref rbuf rpos) nbits)))
             nbits (+ nbits 8)
             rpos (1+ rpos)))))

(defmacro %inflate-fill (need)
  "Like %INFLATE-REFILL but never signals (short-tail peeks)."
  `(loop while (and (< nbits ,need) (< rpos rend)) do
     #+(and sbcl cl-newzlib-le)
     (loop while (and (< nbits 17) (<= (+ rpos 4) rend)) do
       (setf accum (logior accum
                           (definitely-the (unsigned-byte 64)
                             (ash (%word-at rsap rpos) nbits)))
             nbits (+ nbits 32)
             rpos (+ rpos 4)))
     (when (< rpos rend)
       (setf accum (logior accum
                           (definitely-the (unsigned-byte 64)
                             (ash (aref rbuf rpos) nbits)))
             nbits (+ nbits 8)
             rpos (1+ rpos)))))

(defmacro %inflate-take (n)
  "Consume N bits (caller guarantees NBITS >= N); returns a fixnum."
  `(prog1 (definitely-the fixnum (logand accum (aref +low-bit-masks+ ,n)))
     (setf accum (definitely-the (unsigned-byte 64) (ash accum (- ,n)))
           nbits (- nbits ,n))))

(defmacro %inflate-decode-slow (root counts first syms idx target)
  "Canonical bit walk for codes longer than ROOT; caller already peeked."
  `(let ((code (reverse-bits (logand accum (aref +low-bit-masks+ ,root)) ,root))
         (index ,idx)
         (len ,root))
     (declare (type fixnum code index len))
     (%inflate-refill ,root)
     (setf accum (definitely-the (unsigned-byte 64) (ash accum (- ,root)))
           nbits (- nbits ,root))
     (loop
       (incf len)
       (when (> len +max-code-length+)
         (error 'newzlib-format-error :detail "invalid Huffman code"))
       (%inflate-refill 1)
       (setf code (logior (ash code 1) (logand accum 1))
             accum (definitely-the (unsigned-byte 64) (ash accum -1))
             nbits (1- nbits))
       (let ((count (aref ,counts len)))
         (declare (type fixnum count))
         (when (< (- code count) (aref ,first len))
           (setf ,target (aref ,syms (+ index (- code (aref ,first len)))))
           (return))
         (setf index (+ index count))))))

(defmacro %inflate-decode-one (root fast counts first syms idx target)
  "Decode one symbol into TARGET (fast table, slow walk fallback)."
  `(progn
     (%inflate-fill ,root)
     (let ((entry (aref ,fast (logand accum (aref +low-bit-masks+ ,root)))))
       (declare (type fixnum entry))
       (if (zerop entry)
           (%inflate-decode-slow ,root ,counts ,first ,syms ,idx ,target)
           (let ((clen (ash entry -9)))
             (declare (type fixnum clen))
             ;; Consume only after enough bits are known present: with a
             ;; truncated tail this signals end-of-input instead of
             ;; decoding garbage.
             (%inflate-refill clen)
             (setf accum (definitely-the (unsigned-byte 64) (ash accum (- clen)))
                   nbits (- nbits clen)
                   ,target (logand entry #x1FF)))))))

(declaim (inline %copy-tiny-match %copy-run-match %copy-fresh-match
                 %copy-overlap-match %copy-inflate-match))

(defun %copy-tiny-match (buffer pos src length)
  "Copy a short (<= 8 byte) match; overlap-safe.  Returns the new POS."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum pos src length))
  (loop for k fixnum below length do
    (setf (aref buffer (+ pos k)) (aref buffer (+ src k))))
  (+ pos length))

(defun %copy-run-match (buffer pos length)
  "Copy a distance-1 run.  Returns the new POS."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum pos length))
  (let ((b (aref buffer (1- pos))))
    (fill buffer b :start pos :end (+ pos length)))
  (+ pos length))

(defun %copy-fresh-match (buffer pos src length)
  "Copy a non-overlapping match.  Returns the new POS."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum pos src length))
  (replace buffer buffer :start1 pos :start2 src
           :end1 (+ pos length) :end2 (+ src length))
  (+ pos length))

(defun %copy-overlap-match (buffer pos src distance length)
  "Copy an overlapping match by doubling spans.  Returns the new POS.
Every REPLACE reads a region ending before its destination begins, so
plain forward copying is safe, turning O(LENGTH) into O(LOG) copies."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum pos src distance length))
  (let ((done distance))
    (declare (type fixnum done))
    (replace buffer buffer :start1 pos :start2 src
             :end1 (+ pos done) :end2 (+ src done))
    (loop while (< done length) do
      (let ((chunk (min done (- length done))))
        (declare (type fixnum chunk))
        (replace buffer buffer :start1 (+ pos done) :start2 pos
                 :end1 (+ pos done chunk) :end2 (+ pos chunk))
        (incf done chunk))))
  (+ pos length))

(defun %copy-inflate-match (buffer pos src distance length)
  "Copy one match; returns the new POS.  Tiny scalar loops (the common
case: ~60% of matches are <= 8 bytes) beat REPLACE call overhead."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum pos src distance length))
  (cond ((<= length 8) (%copy-tiny-match buffer pos src length))
        ((= distance 1) (%copy-run-match buffer pos length))
        ((<= length distance) (%copy-fresh-match buffer pos src length))
        (t (%copy-overlap-match buffer pos src distance length))))

(defmacro %emit-inflate-match ()
  "Decode length/distance extras and copy one match.  Uses the loop locals
SYM DSYM BUFFER SIZE POS (rebinding all three) plus the bit/table locals."
  `(progn
     (when (> sym 285)
       (error 'newzlib-format-error :detail "invalid length code"))
     ;; one combined load yields base and extra-bits count
     (let* ((be (length-base+extra (- sym 257)))
            (eb (ash be -16)))
       (declare (type (unsigned-byte 32) be) (type fixnum eb))
       (%inflate-refill eb)
       (let ((length (+ (logand be #xFFFF) (%inflate-take eb))))
         (declare (type fixnum length))
         (%inflate-decode-one droot dfast dcounts dfirst dsyms didx dsym)
         (when (> dsym 29)
           (error 'newzlib-format-error :detail "invalid distance code"))
         (let* ((bde (dist-base+extra dsym))
                (deb (ash bde -16)))
           (declare (type (unsigned-byte 32) bde) (type fixnum deb))
           (%inflate-refill deb)
           (let ((distance (+ (logand bde #xFFFF) (%inflate-take deb))))
             (declare (type fixnum distance))
             (when (> distance pos)
               (error 'newzlib-format-error
                      :detail "match distance exceeds output"))
             (let ((src (- pos distance)))
               (declare (type fixnum src))
               (multiple-value-bind (nbuffer nsize)
                   (ensure-out-capacity buffer size pos length)
                 (setf buffer nbuffer size nsize
                       pos (%copy-inflate-match buffer pos src distance length))))))))))

(defmacro %with-inflate-tables ((lit dist) &body body)
  "Bind LIT/DIST decode-table locals (LROOT..DIDX) with types around BODY."
  `(let ((lroot (hdt-root ,lit)) (lfast (hdt-fast ,lit))
         (lcounts (hdt-counts ,lit)) (lfirst (hdt-first ,lit))
         (lsyms (hdt-symbols ,lit)) (lidx (hdt-index-root ,lit))
         (droot (hdt-root ,dist)) (dfast (hdt-fast ,dist))
         (dcounts (hdt-counts ,dist)) (dfirst (hdt-first ,dist))
         (dsyms (hdt-symbols ,dist)) (didx (hdt-index-root ,dist)))
     (declare (type fixnum lroot lidx droot didx)
              (type (simple-array fixnum (*)) lfast lcounts lfirst lsyms
                    dcounts dfirst dsyms))
     ,@body))

(defun inflate-token-stream (reader buffer size pos lit dist)
  "Decode literal/length-distance tokens into BUFFER[POS..] via LIT/DIST.
Returns (VALUES BUFFER POS SIZE).  Reader state stays in locals (like C
inflate_fast); it is written back on end-of-block, stale on error abort."
  (declare (optimize (speed 3) (safety 0))
           (type huffman-decode-table lit dist)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size pos))
  (let ((accum (br-accum reader)) (nbits (br-nbits reader))
        (rpos (br-pos reader)) (rend (br-end reader))
        (rbuf (br-buffer reader)) (sym 0) (dsym 0))
    (declare (type (unsigned-byte 64) accum)
             (type fixnum nbits rpos rend sym dsym)
             (type (simple-array (unsigned-byte 8) (*)) rbuf))
    ;; Pin the input once: refills below allocate nothing (no per-refill
    ;; SAP consing), so no GC intervenes; output growth never moves RBUF.
    (with-pinned-input (rsap rbuf)
      (%with-inflate-tables (lit dist)
        (loop
          (%inflate-decode-one lroot lfast lcounts lfirst lsyms lidx sym)
          (cond ((< sym 256)
                 (multiple-value-bind (nbuffer nsize)
                     (ensure-out-capacity buffer size pos 1)
                   (setf buffer nbuffer size nsize)
                   (setf (aref buffer pos) sym)
                   (incf pos)))
                ((= sym 256)
                 (setf (br-accum reader) accum
                       (br-nbits reader) nbits
                       (br-pos reader) rpos)
                 (return (values buffer pos size)))
                (t (%emit-inflate-match))))))))
;;; ------------------------------------------------------------------
;;; Dynamic block header
;;; ------------------------------------------------------------------

(defun %read-cl-repeat (reader sym i total lengths)
  "Apply code-length repeat SYM at index I of LENGTHS; returns the new I."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum sym i total)
           (type (simple-array fixnum (*)) lengths))
  (labels ((fill-run (value rep)
             (declare (type fixnum value rep))
             (dotimes (k rep)
               (declare (ignore k))
               (when (>= i total)
                 (error 'newzlib-format-error
                        :detail "code length repeat overruns table"))
               (setf (aref lengths i) value)
               (incf i))))
    (cond ((= sym 16)
           (when (zerop i)
             (error 'newzlib-format-error
                    :detail "repeat code 16 with no previous length"))
           (fill-run (aref lengths (1- i)) (+ (read-bits reader 2) 3)))
          (t (fill-run 0 (+ (read-bits reader (if (= sym 17) 3 7))
                            (if (= sym 17) 3 11))))))
  i)

(defun inflate-dynamic-header (reader)
  "Decode the dynamic block header, returning (VALUES LIT DIST) decode tables."
  (declare (optimize (speed 3) (safety 0)))
  (let ((hlit (+ (read-bits reader 5) 257))
        (hdist (+ (read-bits reader 5) 1))
        (hclen (+ (read-bits reader 4) 4)))
    (declare (type fixnum hlit hdist hclen))
    (let ((cl-lengths (make-array 19 :element-type 'fixnum :initial-element 0)))
      (dotimes (i hclen)
        (setf (aref cl-lengths (aref +code-length-order+ i)) (read-bits reader 3)))
      (let ((cl-tree (build-huffman-decode-table cl-lengths))
            (lengths (make-array (+ hlit hdist) :element-type 'fixnum
                                 :initial-element 0))
            (total (+ hlit hdist))
            (i 0))
        (declare (type fixnum total i))
        (loop while (< i total) do
          (let ((sym (huffman-decode cl-tree reader)))
            (declare (type fixnum sym))
            (cond ((< sym 16) (setf (aref lengths i) sym) (incf i))
                  ((<= 16 sym 18)
                   (setf i (%read-cl-repeat reader sym i total lengths)))
                  (t (error 'newzlib-format-error
                            :detail "invalid code length code")))))
        (values (build-huffman-decode-table lengths 0 hlit)
                (build-huffman-decode-table lengths hlit hdist))))))

;;; ------------------------------------------------------------------
;;; Block driver
;;; ------------------------------------------------------------------

(defun inflate-blocks (reader buffer size pos)
  "Decode consecutive DEFLATE blocks from READER into BUFFER[POS..].  Returns
(VALUES BUFFER POS SIZE)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size pos))
  (iterate:iterate
    (iterate:for bfinal = (read-bits reader 1))
    (iterate:for btype = (read-bits reader 2))
    (declare (type fixnum bfinal btype))
    (case btype
      (0 (multiple-value-bind (nbuffer npos nsize)
             (inflate-stored-block reader buffer size pos)
           (setf buffer nbuffer
                 pos npos
                 size nsize)))
      (1 (multiple-value-bind (lit dist) (ensure-fixed-tables)
           (multiple-value-bind (nbuffer npos nsize)
               (inflate-token-stream reader buffer size pos lit dist)
             (setf buffer nbuffer
                   pos npos
                   size nsize))))
      (2 (multiple-value-bind (lit dist) (inflate-dynamic-header reader)
           (multiple-value-bind (nbuffer npos nsize)
               (inflate-token-stream reader buffer size pos lit dist)
             (setf buffer nbuffer
                   pos npos
                   size nsize))))
      (otherwise (error 'newzlib-format-error :detail "invalid block type")))
    (when (plusp bfinal)
      (iterate:leave (values buffer pos size))))
  (values buffer pos size))

(defun inflate-raw (input &optional (start 0) (end (length input)))
  "Decompress a raw DEFLATE stream INPUT[START,END).  Returns a fresh
  octet vector."
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end))
  (unless (typep input '(simple-array (unsigned-byte 8) (*)))
    (error 'newzlib-parameter-error :detail "input must be an (unsigned-byte 8) vector"))
  (let ((reader (make-bit-reader input start end))
        (size 1024)
        (pos 0))
    (declare (type fixnum size pos))
    (let ((buffer (make-octet-buffer size)))
      (multiple-value-bind (buffer pos)
          (inflate-blocks reader buffer size pos)
        (let ((result (make-octet-buffer pos)))
          (replace result buffer :end2 pos)
          result)))))
