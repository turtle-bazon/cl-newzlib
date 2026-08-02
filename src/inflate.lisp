(in-package #:cl-newzlib)

;;; DEFLATE decompressor (RFC 1951).
;;;
;;; Reads a raw DEFLATE stream from a bit-reader, handling stored, fixed and
;;; dynamic blocks, and appends decoded output to a growable octet buffer.

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
;;; Stored blocks
;;; ------------------------------------------------------------------

(defun inflate-stored-block (reader out)
  (declare (optimize (speed 3) (safety 0)))
  (align-reader reader)
  (let ((len (read-bits reader 16))
        (nlen (read-bits reader 16)))
    (unless (= (logand (lognot len) #xFFFF) nlen)
      (error 'newzlib-format-error :detail "stored block length mismatch"))
    (loop repeat len do
      (vector-push-extend (read-bits reader 8) out)))
  nil)

;;; ------------------------------------------------------------------
;;; Token stream (fixed and dynamic blocks)
;;; ------------------------------------------------------------------

(defun inflate-token-stream (reader out lit dist)
  "Decode literal/length-distance tokens from READER using LIT and DIST
  decode tables, appending output bytes to OUT until the end-of-block code."
  (declare (optimize (speed 3) (safety 0))
           (type huffman-decode-table lit dist))
  (loop do
    (let ((sym (huffman-decode lit reader)))
      (declare (type fixnum sym))
      (cond
        ((< sym 256)
         (vector-push-extend sym out))
        ((= sym 256)
         (return))
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
             (when (> distance (length out))
               (error 'newzlib-format-error :detail "match distance exceeds output"))
             (let ((src (- (length out) distance)))
               (declare (type fixnum src))
               (loop repeat length do
                 (vector-push-extend (aref out src) out)
                 (incf src)))))))))
  nil)

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
          (loop while (< i (+ hlit hdist)) do
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
                     (loop repeat rep do
                       (when (>= i (+ hlit hdist))
                         (error 'newzlib-format-error
                                :detail "code length repeat overruns table"))
                       (setf (aref lengths i) prev)
                       (incf i)))))
                ((or (= sym 17) (= sym 18))
                 (let ((rep (+ (read-bits reader (if (= sym 17) 3 7))
                               (if (= sym 17) 3 11))))
                   (declare (type fixnum rep))
                   (loop repeat rep do
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

(defun inflate-blocks (reader out)
  "Decode consecutive DEFLATE blocks from READER until the final block."
  (declare (optimize (speed 3) (safety 0)))
  (loop do
    (let ((bfinal (read-bits reader 1))
          (btype (read-bits reader 2)))
      (declare (type fixnum bfinal btype))
      (case btype
        (0 (inflate-stored-block reader out))
        (1 (multiple-value-bind (lit dist) (ensure-fixed-tables)
             (inflate-token-stream reader out lit dist)))
        (2 (multiple-value-bind (lit dist) (inflate-dynamic-header reader)
             (inflate-token-stream reader out lit dist)))
        (otherwise (error 'newzlib-format-error :detail "invalid block type")))
      (when (plusp bfinal)
        (return))))
  nil)

(defun inflate-raw (input &optional (start 0) (end (length input)))
  "Decompress a raw DEFLATE stream INPUT[START,END).  Returns a fresh
  octet vector."
  (declare (type simple-array input)
           (type fixnum start end))
  (unless (typep input '(simple-array (unsigned-byte 8) (*)))
    (error 'newzlib-parameter-error :detail "input must be an (unsigned-byte 8) vector"))
  (let ((reader (make-bit-reader input start end))
        (out (make-growable-buffer 1024)))
    (inflate-blocks reader out)
    (let ((result (make-octet-buffer (length out))))
      (replace result out)
      result)))
