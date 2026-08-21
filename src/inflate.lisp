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

(defparameter +fixed-lit-table+ nil)
(defparameter +fixed-dist-table+ nil)

(defun ensure-fixed-tables ()
  (unless +fixed-lit-table+
    (setf +fixed-lit-table+ (build-huffman-decode-table +fixed-lit-lengths+)
          +fixed-dist-table+ (build-huffman-decode-table +fixed-dist-lengths+)))
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
      (let ((new-size size))
        (loop while (< new-size (+ pos need)) do (setf new-size (* 2 new-size)))
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

(defun inflate-token-stream (reader buffer size pos lit dist)
  "Decode literal/length-distance tokens from READER using LIT and DIST
decode tables into BUFFER[POS..].  Returns (VALUES BUFFER POS)."
  (declare (optimize (speed 3) (safety 0))
           (type huffman-decode-table lit dist)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size pos))
  (iterate:iterate
    (iterate:for sym = (huffman-decode lit reader))
    (declare (type fixnum sym))
    (cond
      ((< sym 256)
       (multiple-value-bind (nbuffer nsize)
           (ensure-out-capacity buffer size pos 1)
         (setf buffer nbuffer
               size nsize)
         (setf (aref buffer pos) sym)
         (incf pos)))
      ((= sym 256)
       (iterate:leave (values buffer pos size)))
      (t
       (when (> sym 285)
         (error 'newzlib-format-error :detail "invalid length code"))
       (let* ((code (- sym 257))
              (length (+ (length-base code) (read-bits reader (length-extra-bits code))))
              (dcode (huffman-decode dist reader)))
         (declare (type fixnum code length dcode))
         (when (> dcode 29)
           (error 'newzlib-format-error :detail "invalid distance code"))
         (let ((distance (+ (dist-base dcode)
                            (read-bits reader (dist-extra-bits dcode)))))
           (declare (type fixnum distance))
           (when (> distance pos)
             (error 'newzlib-format-error :detail "match distance exceeds output"))
           (let ((src (- pos distance)))
             (declare (type fixnum src))
             (multiple-value-bind (nbuffer nsize)
                 (ensure-out-capacity buffer size pos length)
               (setf buffer nbuffer
                     size nsize)
                 (cond
                   ;; run-length copy: every byte repeats the one before
                   ((= distance 1)
                    (let ((b (aref buffer (1- pos))))
                      (fill buffer b :start pos :end (+ pos length)))
                    (incf pos length))
                   ;; non-overlapping copy
                   ((<= length distance)
                    (replace buffer buffer
                             :start1 pos :start2 src
                             :end1 (+ pos length) :end2 (+ src length))
                    (incf pos length))
                   ;; overlapping copy: each byte reads the byte DISTANCE
                   ;; back, which this same copy has already written
                   (t
                    (iterate:iterate
                      (iterate:for i from pos below (+ pos length))
                      (setf (aref buffer i) (aref buffer (- i distance))))
                    (incf pos length))))))))))
  (values buffer pos size))
;;; ------------------------------------------------------------------
;;; Dynamic block header
;;; ------------------------------------------------------------------

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
                                 :initial-element 0)))
        (let ((i 0))
          (declare (type fixnum i))
          (iterate:iterate
            (iterate:while (< i (+ hlit hdist)))
            (let ((sym (huffman-decode cl-tree reader)))
              (declare (type fixnum sym))
              (cond
                ((< sym 16)
                 (setf (aref lengths i) sym)
                 (incf i))
                ((= sym 16)
                 (when (zerop i)
                   (error 'newzlib-format-error
                          :detail "repeat code 16 with no previous length"))
                 (let ((rep (+ (read-bits reader 2) 3)))
                   (declare (type fixnum rep))
                   (let ((prev (aref lengths (1- i))))
                     (iterate:iterate (iterate:repeat rep)
                       (when (>= i (+ hlit hdist))
                         (error 'newzlib-format-error
                                :detail "code length repeat overruns table"))
                       (setf (aref lengths i) prev)
                       (incf i)))))
                ((or (= sym 17) (= sym 18))
                 (let ((rep (+ (read-bits reader (if (= sym 17) 3 7))
                               (if (= sym 17) 3 11))))
                   (declare (type fixnum rep))
                   (iterate:iterate (iterate:repeat rep)
                     (when (>= i (+ hlit hdist))
                       (error 'newzlib-format-error
                              :detail "code length repeat overruns table"))
                     (setf (aref lengths i) 0)
                     (incf i))))
                (t
                 (error 'newzlib-format-error
                        :detail "invalid code length code"))))))
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
