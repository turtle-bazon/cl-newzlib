(in-package #:cl-newzlib)

;;; Streaming API.
;;;
;;; DEFLATE-STREAM accumulates input octets and produces the compressed
;;; output in one pass on DEFLATE-STREAM-FINISH.  INFLATE-STREAM lazily
;;; decompresses its whole input on the first read and serves it out in
;;; chunks.

(defstruct (deflate-stream
            (:constructor %make-deflate-stream)
            (:conc-name ds-)
            (:predicate nil)
            (:copier nil))
  (buffer nil :type (or null (array (unsigned-byte 8) (*)))) ; growable octet buffer
  (level +default-compression+ :type fixnum)
  (finished nil :type boolean)
  (result nil :type (or null simple-array)))

(defun make-deflate-stream (&key (level +default-compression+))
  "Create a compression stream at LEVEL.  Feed it with
  DEFLATE-STREAM-WRITE, then produce the compressed output with
  DEFLATE-STREAM-FINISH."
  (check-compression-level level)
  (%make-deflate-stream :buffer (make-growable-buffer 1024) :level level))

(defun deflate-stream-write (stream octets &optional (start 0) (end (length octets)))
  "Append OCTETS[START,END) to the input of STREAM."
  (declare (type simple-array octets)
           (type fixnum start end))
  (when (ds-finished stream)
    (error 'newzlib-parameter-error :detail "write after deflate-stream-finish"))
  (let ((buffer (ds-buffer stream)))
    (loop for i from start below end do
      (vector-push-extend (aref octets i) buffer)))
  stream)

(defun deflate-stream-finish (stream)
  "Compress all buffered input and return the compressed octets.  Only the
  first call compresses; later calls return the same result."
  (unless (ds-finished stream)
    (let* ((buffer (ds-buffer stream))
           (n (length buffer))
           (input (if (typep buffer '(simple-array (unsigned-byte 8) (*)))
                      buffer
                      (subseq buffer 0 n))))
      (setf (ds-result stream) (deflate-raw input 0 n (ds-level stream))
            (ds-finished stream) t)))
  (ds-result stream))

(defun deflate-stream-end (stream)
  "Release the resources held by STREAM."
  (setf (ds-buffer stream) nil
        (ds-result stream) nil)
  nil)

(defstruct (inflate-stream
            (:constructor %make-inflate-stream)
            (:conc-name is-)
            (:predicate nil)
            (:copier nil))
  (input nil :type (or null simple-array)) ; compressed octets
  (decoded nil :type (or null simple-array)) ; decompressed octets (lazy)
  (pos 0 :type fixnum)                    ; read offset into DECODED
  (ready nil :type boolean))              ; whether DECODED has been filled

(defun make-inflate-stream (octets &optional (start 0) (end (length octets)))
  "Create a decompression stream over the compressed octets OCTETS[START,END)."
  (declare (type simple-array octets)
           (type fixnum start end))
  (%make-inflate-stream :input (subseq octets start end)))

(defun inflate-stream-ensure-decoded (stream)
  (unless (is-ready stream)
    (setf (is-decoded stream) (inflate-raw (is-input stream))
          (is-ready stream) t))
  nil)

(defun inflate-stream-read (stream count)
  "Read up to COUNT decompressed octets from STREAM, returning a fresh octet
  vector (possibly shorter, empty once input is exhausted)."
  (declare (type fixnum count))
  (inflate-stream-ensure-decoded stream)
  (let* ((decoded (is-decoded stream))
         (remaining (- (length decoded) (is-pos stream)))
         (n (min count remaining)))
    (let ((out (make-octet-buffer n)))
      (when (plusp n)
        (replace out decoded :start2 (is-pos stream))
        (incf (is-pos stream) n))
      out)))

(defun inflate-stream-eof-p (stream)
  "Return true when all decompressed octets have been read."
  (inflate-stream-ensure-decoded stream)
  (>= (is-pos stream) (length (is-decoded stream))))

(defun inflate-stream-end (stream)
  "Release the resources held by STREAM."
  (setf (is-input stream) nil
        (is-decoded stream) nil)
  nil)
