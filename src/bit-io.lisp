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
  (buffer nil :type (simple-array (unsigned-byte 8) (*))) ; output octets
  (size 0 :type fixnum)             ; capacity of buffer
  (pos 0 :type fixnum)              ; number of bytes committed
  (accum 0 :type (unsigned-byte 64)); pending bits, low NBITS significant
  (nbits 0 :type fixnum))           ; number of pending bits in ACCUM

(defun make-bit-writer (initial-size)
  (%make-bit-writer :buffer (make-octet-buffer initial-size)
                    :size initial-size))

;;; Precomputed (1- (ash 1 count)) for COUNT in 0..63, as unsigned-byte 64.
;;; Indexing this table avoids the bignum-allocation guard SBCL would emit
;;; for an unconstrained (ASH 1 COUNT) on every bit-I/O call.
(defvar +low-bit-masks+
  (coerce (loop for i from 0 below 64
                collect (ldb (byte 64 0) (1- (ash 1 i))))
          '(simple-array (unsigned-byte 64) (64))))

(declaim (type (simple-array (unsigned-byte 64) (64)) +low-bit-masks+))

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

;;; Little-endian machines can move whole words between the accumulator and
;;; the output buffer; DEFLATE's LSB-first packing is exactly little-endian
;;; byte order, so a native 32-bit store is equivalent to four byte stores.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (when (eq sb-c:*backend-byte-order* :little-endian)
           (pushnew :cl-newzlib-le *features*)))

#+(and sbcl cl-newzlib-le)
(progn
  (declaim (inline %word-at))
  (defun %word-at (base i)
    "Load the native (little-endian) 32-bit word at byte offset I through
the system-area pointer BASE."
    (declare (optimize (speed 3) (safety 0))
             (type sb-sys:system-area-pointer base)
             (type fixnum i))
    (sb-sys:sap-ref-32 base i)))

(declaim (inline flush-pending-bytes))
(defun flush-pending-bytes (writer)
  "Flush as many whole bytes as possible from the accumulator into the buffer."
  (declare (optimize (speed 3) (safety 0)))
  (let ((accum (bw-accum writer))
        (nbits (bw-nbits writer))
        (pos (bw-pos writer))
        (buffer (bw-buffer writer))
        (size (bw-size writer)))
    (declare (type (unsigned-byte 64) accum)
             (type fixnum nbits pos size)
             (type (simple-array (unsigned-byte 8) (*)) buffer))
    (macrolet ((grow ()
                 '(let ((bigger (make-octet-buffer (* 2 size))))
                    (replace bigger buffer :end2 size)
                    (setq buffer bigger
                          size (* 2 size)))))
      #+(and sbcl cl-newzlib-le)
      (progn
        (loop while (>= nbits 32) do
          (when (> (+ pos 4) size) (grow))
          ;; no allocation happens between taking the SAP and the store, so
          ;; the vector cannot move out from under it even without pinning
          (setf (sb-sys:sap-ref-32 (sb-sys:vector-sap buffer) pos)
                (ldb (byte 32 0) accum))
          (setf accum (ash accum -32)
                nbits (- nbits 32)
                pos (+ pos 4))))
      (loop while (>= nbits 8) do
        (when (>= pos size) (grow))
        (setf (aref buffer pos) (logand accum #xFF))
        (setf accum (ash accum -8)
              nbits (- nbits 8)
              pos (1+ pos))))
    (setf (bw-accum writer) accum
          (bw-nbits writer) nbits
          (bw-pos writer) pos
          (bw-buffer writer) buffer
          (bw-size writer) size))
  nil)

(declaim (inline write-bits))
(defun write-bits (writer bits count)
  "Append the low COUNT bits of BITS to WRITER, LSB-first.  COUNT must be
at most 16 (the largest count DEFLATE ever emits is a stored block's
16-bit length); the accumulator invariant keeps NBITS below 8 between
calls, so the shifted value always fits in 24 bits and no bignum guard is
needed."
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 64) bits)
           (type (integer 0 16) count))
  (let ((nbits (bw-nbits writer)))
    (declare (type (unsigned-byte 4) nbits))
    (setf (bw-accum writer)
          (definitely-the
              (unsigned-byte 64)
            (logior (bw-accum writer)
                    (ash (definitely-the (unsigned-byte 16)
                           (ldb (byte count 0) bits))
                         nbits)))
          (bw-nbits writer) (+ nbits count))
    (when (>= (+ nbits count) 8)
      (flush-pending-bytes writer)))
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
  (buffer nil :type (simple-array (unsigned-byte 8) (*))) ; input octets
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
        (end (br-end reader))
        (buffer (br-buffer reader))
        (accum (br-accum reader))
        (nbits (br-nbits reader)))
    (declare (type fixnum pos end nbits)
             (type (unsigned-byte 64) accum)
             (type (simple-array (unsigned-byte 8) (*)) buffer))
    (when (>= pos end)
      (error 'newzlib-end-of-input))
    ;; little-endian fast path: pull a whole word when it fits safely
    #+(and sbcl cl-newzlib-le)
    ;; no allocation occurs inside this loop, so the SAP stays valid even
    ;; though BUFFER is not pinned
    (loop while (and (<= nbits 24) (<= (+ pos 4) end))
          do (setf accum (logior accum
                                 (definitely-the (unsigned-byte 64)
                                   (ash (%word-at (sb-sys:vector-sap buffer)
                                                  pos)
                                        nbits)))
                 nbits (+ nbits 32)
                 pos (+ pos 4)))
    (when (< pos end)
      (setf accum (logior accum
                          (definitely-the (unsigned-byte 64)
                            (ash (aref buffer pos) nbits)))
            nbits (+ nbits 8)
            pos (1+ pos)))
    (setf (br-accum reader) accum
          (br-pos reader) pos
          (br-nbits reader) nbits))
  nil)

(declaim (inline peek-bits))
(defun peek-bits (reader count)
  "Return the next COUNT bits of READER without consuming them.  COUNT must
be <= 48 when bytes remain, else error is signalled on refill."
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 6) count))
  (loop while (< (br-nbits reader) count) do
    (refill-reader reader))
  (logand (br-accum reader) (aref +low-bit-masks+ count)))

(declaim (inline peek-bits-capped))
(defun peek-bits-capped (reader count)
  "Peek up to COUNT bits, refilling as possible without signalling on end
of input.  Returns (VALUES VALUE AVAILABLE) where AVAILABLE is the number
of valid bits in VALUE; VALUE's high bits beyond AVAILABLE are zero."
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 6) count))
  (let ((nbits (br-nbits reader)))
    (declare (type fixnum nbits))
    (when (< nbits count)
      (let ((pos (br-pos reader))
            (end (br-end reader))
            (buffer (br-buffer reader))
            (accum (br-accum reader)))
        (declare (type fixnum pos end)
                 (type (unsigned-byte 64) accum)
                 (type (simple-array (unsigned-byte 8) (*)) buffer))
        #+(and sbcl cl-newzlib-le)
        (loop while (and (< nbits 17) (<= (+ pos 4) end))
              do (setf accum (logior accum
                                     (definitely-the (unsigned-byte 64)
                                       (ash (%word-at (sb-sys:vector-sap buffer)
                                                      pos)
                                            nbits)))
                     nbits (+ nbits 32)
                     pos (+ pos 4)))
        (loop while (and (< nbits count) (< pos end))
              do (setf accum (logior accum
                                     (definitely-the (unsigned-byte 64)
                                       (ash (aref buffer pos) nbits)))
                     nbits (+ nbits 8)
                     pos (1+ pos)))
        (setf (br-accum reader) accum
              (br-pos reader) pos
              (br-nbits reader) nbits)))
    (values (logand (br-accum reader) (aref +low-bit-masks+ count)) nbits)))

(declaim (inline read-bits))
(defun read-bits (reader count)
  "Consume and return the next COUNT bits of READER, LSB-first.  COUNT must
be at most 63."
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 6) count))
  (loop while (< (br-nbits reader) count) do
    (refill-reader reader))
  (prog1 (logand (br-accum reader) (aref +low-bit-masks+ count))
    (setf (br-accum reader)
          (definitely-the
              (unsigned-byte 64)
            (ash (br-accum reader) (- count)))
          (br-nbits reader) (- (br-nbits reader) count))))
