(in-package #:cl-newzlib)

;;; Optional SIMD support (SBCL on x86-64): load the bundled sb-simd contrib
;;; up front so later files can compile against it and gate code on the
;;; :newzlib-simd feature.  Everything must still work without it -- other
;;; Lisps, other architectures and SBCL builds missing the contrib all fall
;;; back to portable scalar code.
#+(and sbcl x86-64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case (require :sb-simd) (error () nil))
  (when (find-package :sb-simd-avx2)
    (pushnew :newzlib-simd *features*)))

(declaim (inline ubyte8-ref ubyte8-set))
(declaim (inline octets-copy))

;;; Little-endian machines can move whole words between a bit accumulator
;;; and an octet vector (DEFLATE's LSB-first packing is exactly little-endian
;;; byte order).  Detect once, early, so every later file can gate on it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (when (eq sb-c:*backend-byte-order* :little-endian)
           (pushnew :cl-newzlib-le *features*)))

;;; Implementations whose fixnums hold 33+ bits carry 32-bit checksum words
;;; as immediates, so table-driven slicing CRC/adler loops run without
;;; boxing; narrower implementations keep the split-word scalar fallbacks.
;;; Detected once, early, for the same reason as above.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (> most-positive-fixnum #xFFFFFFFF)
    (pushnew :cl-newzlib-wide-fixnum *features*)))

;;; SBCL-specific: assert an arithmetic RESULT type so the compiler emits a
;;; narrow VOP instead of a generic/bignum-safe fallback for the hot bit-I/O
;;; shifts.  On other implementations it degrades to the standard THE.
(defmacro definitely-the (type form)
  #+sbcl `(sb-ext:truly-the ,type ,form)
  #-sbcl `(the ,type ,form))

;;; Compression level constants, mirroring zlib's Z_NO_COMPRESSION (0),
;;; Z_BEST_SPEED (1), Z_DEFAULT_COMPRESSION (-1) and Z_BEST_COMPRESSION (9).
(defparameter +no-compression+ 0)
(defparameter +best-speed+ 1)
(defparameter +default-compression+ 6)
(defparameter +best-compression+ 9)

;;; Friendly aliases, also mirroring zlib's Z_* level constants.
(defconstant compression-level-no-compression 0)
(defconstant compression-level-fastest 1)
(defconstant compression-level-default 6)
(defconstant compression-level-best 9)

(defun check-compression-level (level)
  (unless (and (integerp level) (<= 0 level 9))
    (error 'newzlib-parameter-error
           :detail (format nil "compression level ~A must be an integer in [0, 9]" level)))
  level)

(defun check-compression-mode (mode)
  (unless (member mode '(:standard :fast))
    (error 'newzlib-parameter-error
           :detail (format nil "unsupported compression mode ~S" mode)))
  mode)

(declaim (ftype (function (t fixnum) (unsigned-byte 8)) ubyte8-ref))
(defun ubyte8-ref (vector index)
  "Fetch byte INDEX of VECTOR (a (unsigned-byte 8) vector) as an
unsigned-byte 8, avoiding generic array access when possible."
  (declare (optimize (speed 3) (safety 0)))
  (aref vector index))

(declaim (ftype (function (t fixnum (unsigned-byte 8)) t) ubyte8-set))
(defun ubyte8-set (vector index value)
  (declare (optimize (speed 3) (safety 0)))
  (setf (aref vector index) value))

(defun octets-copy (src src-start dst dst-start count)
  "Copy COUNT octets from SRC starting at SRC-START into DST at DST-START."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array src dst))
  (replace dst src :start1 dst-start :start2 src-start :end1 (+ dst-start count)
          :end2 (+ src-start count))
  dst)

;;; Shared octet-buffer allocation.  Kept in one place so we can later switch
;;; to specialized allocation (static-vectors, arena, ...) with #+sbcl.
(defun make-octet-buffer (size)
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

(defun make-growable-buffer (initial-size)
  (make-array initial-size
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun version ()
  (asdf:component-version (asdf:find-system "cl-newzlib")))
