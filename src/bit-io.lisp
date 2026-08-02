(in-package #:cl-newzlib)

;;; Bit-level I/O for DEFLATE (RFC 1951).
;;;
;;; DEFLATE reads/writes bits LSB-first within each byte.  Both directions
;;; are built on a fixnum/unsigned accumulator that batches bit work and only
;;; touches the octet vector in byte-sized chunks.  The hot paths are
;;; declared INLINE and written to avoid boxing and generic arithmetic on
;;; SBCL.

;;; ------------------------------------------------------------------
;;; Bit writer
;;; ------------------------------------------------------------------

(defstruct (bit-writer
            (:constructor %make-bit-writer)
            (:conc-name bw-)
            (:predicate nil)
            (:copier nil))
  (buffer nil :type simple-array)   ; simple-array (unsigned-byte 8)
  (size 0 :type fixnum)             ; capacity of buffer
  (pos 0 :type fixnum)              ; number of bytes committed
  (accum 0 :type (unsigned-byte 64)); pending bits, low NBITS significant
  (nbits 0 :type fixnum))           ; number of pending bits in ACCUM

(defun make-bit-writer (initial-size)
  (%make-bit-writer :buffer (make-octet-buffer initial-size)
                    :size initial-size))

(declaim (inline grow-buffer))
(defun grow-buffer (buffer size)
  (let ((new (make-octet-buffer (* 2 size))))
    (replace new buffer :end2 size)
    new))

(declaim (inline ensure-writer-capacity))
(defun ensure-writer-capacity (writer)
  (declare (optimize (speed 3) (safety 0)))
  (when (>= (bw-pos writer) (bw-size writer))
    (let ((bigger (grow-buffer (bw-buffer writer) (bw-size writer))))
      (setf (bw-buffer writer) bigger
            (bw-size writer) (* 2 (bw-size writer)))))
  nil)

(declaim (inline flush-pending-bytes))
(defun flush-pending-bytes (writer)
  "Flush as many whole bytes as possible from the accumulator into the buffer."
  (declare (optimize (speed 3) (safety 0)))
  (let ((accum (bw-accum writer))
        (nbits (bw-nbits writer))
        (pos (bw-pos writer))
        (buffer (bw-buffer writer)))
    (declare (type (unsigned-byte 64) accum)
             (type fixnum nbits pos))
    (loop while (>= nbits 8) do
      (when (>= pos (bw-size writer))
        (setf buffer (grow-buffer buffer (bw-size writer)))
        (setf (bw-buffer writer) buffer)
        (setf (bw-size writer) (* 2 (bw-size writer))))
      (setf (aref buffer pos) (logand accum #xFF))
      (setf accum (ash accum -8)
            nbits (- nbits 8)
            pos (1+ pos)))
    (setf (bw-accum writer) accum
          (bw-nbits writer) nbits
          (bw-pos writer) pos))
  nil)

(declaim (inline write-bits))
(defun write-bits (writer bits count)
  "Append the low COUNT bits of BITS to WRITER, LSB-first."
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 64) bits)
           (type fixnum count))
  (setf (bw-accum writer)
        (logior (bw-accum writer)
                (ash (logand bits (1- (ash 1 count)))
                     (bw-nbits writer)))
        (bw-nbits writer) (+ (bw-nbits writer) count))
  (when (>= (bw-nbits writer) 8)
    (flush-pending-bytes writer))
  nil)

(declaim (inline flush-bits))
(defun flush-bits (writer)
  "Zero-pad the accumulator to a byte boundary and flush it."
  (declare (optimize (speed 3) (safety 0)))
  (when (plusp (bw-nbits writer))
    (setf (bw-nbits writer) 8)
    (flush-pending-bytes writer))
  nil)

(declaim (inline writer-bytes))
(defun writer-bytes (writer)
  "Return the committed bytes as a fresh simple octet vector."
  (declare (optimize (speed 3) (safety 0)))
  (flush-bits writer)
  (let ((out (make-octet-buffer (bw-pos writer))))
    (replace out (bw-buffer writer) :end2 (bw-pos writer))
    out))

;;; ------------------------------------------------------------------
;;; Bit reader
;;; ------------------------------------------------------------------

(defstruct (bit-reader
            (:constructor %make-bit-reader)
            (:conc-name br-)
            (:predicate nil)
            (:copier nil))
  (buffer nil :type simple-array)   ; input octet vector
  (pos 0 :type fixnum)              ; next byte index in buffer
  (end 0 :type fixnum)              ; one past last available byte
  (accum 0 :type (unsigned-byte 64)); pending bits, low NBITS significant
  (nbits 0 :type fixnum))           ; number of valid bits in ACCUM

(defun make-bit-reader (buffer &optional (start 0) (end (length buffer)))
  (%make-bit-reader :buffer buffer :pos start :end end))

(declaim (inline refill-reader))
(defun refill-reader (reader)
  "Load more bytes into the accumulator.  Returns nil; signals
NEWZLIB-END-OF-INPUT when no bytes remain."
  (declare (optimize (speed 3) (safety 0)))
  (let ((pos (br-pos reader))
        (end (br-end reader)))
    (when (>= pos end)
      (error 'newzlib-end-of-input))
    (setf (br-accum reader)
          (logior (br-accum reader)
                  (ash (aref (br-buffer reader) pos) (br-nbits reader)))
          (br-pos reader) (1+ pos)
          (br-nbits reader) (+ (br-nbits reader) 8)))
  nil)

(declaim (inline peek-bits))
(defun peek-bits (reader count)
  "Return the next COUNT bits of READER without consuming them.  COUNT must
be <= 48 when bytes remain, else error is signalled on refill."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum count))
  (loop while (< (br-nbits reader) count) do
    (refill-reader reader))
  (logand (br-accum reader) (1- (ash 1 count))))

(declaim (inline read-bits))
(defun read-bits (reader count)
  "Consume and return the next COUNT bits of READER, LSB-first."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum count))
  (loop while (< (br-nbits reader) count) do
    (refill-reader reader))
  (prog1 (logand (br-accum reader) (1- (ash 1 count)))
    (setf (br-accum reader) (ash (br-accum reader) (- count))
          (br-nbits reader) (- (br-nbits reader) count))))
