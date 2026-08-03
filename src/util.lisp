(in-package #:cl-newzlib)

(declaim (inline ubyte8-ref ubyte8-set))
(declaim (inline octets-copy))

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
