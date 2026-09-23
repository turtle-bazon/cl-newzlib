(in-package #:cl-newzlib)

;;; RFC 1952 CRC-32, the IEEE 802.3 polynomial reflected as 0xEDB88320,
;;; with the standard init 0xFFFFFFFF and final xor 0xFFFFFFFF.

(defun compute-crc32-table ()
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256)
      (let ((c n))
        (declare (type (unsigned-byte 32) c))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c)
                      (logxor #xEDB88320 (ash c -1))
                      (ash c -1))))
        (setf (aref table n) c)))
    table))

(defvar +crc32-table+ (compute-crc32-table))

;;; Slicing-by-16 tables (Intel's algorithm, as in zlib's crc32_braid):
;;; T[k][b] is the CRC of byte b followed by k zero bytes, so sixteen input
;;; bytes fold through sixteen parallel table lookups per iteration instead
;;; of one lookup per byte.  T[0] is the standard table above.  Sixteen
;;; tables (16 KiB) still fit comfortably in L1 cache.
(defun compute-crc32-slice-tables ()
  (let ((tables (make-array 16)))
    (setf (aref tables 0) +crc32-table+)
    (dotimes (k 15)
      (let ((prev (aref tables k))
            (cur (make-array 256 :element-type '(unsigned-byte 32))))
        (dotimes (n 256)
          (let ((c (aref prev n)))
            (declare (type (unsigned-byte 32) c))
            (setf (aref cur n)
                  (logxor (aref +crc32-table+ (logand c #xFF))
                          (ash c -8)))))
        (setf (aref tables (1+ k)) cur)))
    tables))

(defvar +crc32-slice+ (compute-crc32-slice-tables))

(declaim (type (simple-vector 16) +crc32-slice+))

;;; Shared slicing folds (Intel's braid, as in zlib's crc32_braid): the two
;;; slicer variants below differ only in how they load 32-bit words
;;; (pinned SAP reads vs portable byte assembly) and in pinning, so the
;;; fold bodies live here once, always inlined into the hot loops.
;;; ------------------------------------------------------------------

(declaim (inline %crc32-fold16 %crc32-fold8 %crc32-tail8))

(defun %crc32-fold16 (c w1 w2 w3 t0 t1 t2 t3 t4 t5 t6 t7
                      t8 t9 t10 t11 t12 t13 t14 t15)
  "Fold sixteen bytes through tables 15..0; C already holds W0 xored in."
  (declare (type (unsigned-byte 32) c w1 w2 w3)
           (type (simple-array (unsigned-byte 32) (*))
                 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logxor (aref t15 (logand c #xFF))
          (aref t14 (logand (ash c -8) #xFF))
          (aref t13 (logand (ash c -16) #xFF))
          (aref t12 (ash c -24))
          (aref t11 (logand w1 #xFF))
          (aref t10 (logand (ash w1 -8) #xFF))
          (aref t9 (logand (ash w1 -16) #xFF))
          (aref t8 (ash w1 -24))
          (aref t7 (logand w2 #xFF))
          (aref t6 (logand (ash w2 -8) #xFF))
          (aref t5 (logand (ash w2 -16) #xFF))
          (aref t4 (ash w2 -24))
          (aref t3 (logand w3 #xFF))
          (aref t2 (logand (ash w3 -8) #xFF))
          (aref t1 (logand (ash w3 -16) #xFF))
          (aref t0 (ash w3 -24))))

(defun %crc32-fold8 (c w1 t0 t1 t2 t3 t4 t5 t6 t7)
  "Fold eight bytes through tables 7..0; C already holds W0 xored in."
  (declare (type (unsigned-byte 32) c w1)
           (type (simple-array (unsigned-byte 32) (*))
                 t0 t1 t2 t3 t4 t5 t6 t7)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logxor (aref t7 (logand c #xFF))
          (aref t6 (logand (ash c -8) #xFF))
          (aref t5 (logand (ash c -16) #xFF))
          (aref t4 (ash c -24))
          (aref t3 (logand w1 #xFF))
          (aref t2 (logand (ash w1 -8) #xFF))
          (aref t1 (logand (ash w1 -16) #xFF))
          (aref t0 (ash w1 -24))))

(defun %crc32-tail8 (c octets i end t0)
  "Scalar tail for fewer than 8 remaining bytes; returns the new C."
  (declare (type (unsigned-byte 32) c)
           (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum i end)
           (type (simple-array (unsigned-byte 32) (*)) t0)
           (optimize (speed 3) (safety 0) (debug 0)))
  (loop for j fixnum from i below end do
    (setf c (logxor (aref t0 (logand #xFF (logxor c (aref octets j))))
                    (ash c -8))))
  c)

(defmacro with-crc32-tables (&body body)
  "Bind T0..T15 to the slicing tables (typed) around BODY."
  `(let ((t0 (aref +crc32-slice+ 0))
         (t1 (aref +crc32-slice+ 1))
         (t2 (aref +crc32-slice+ 2))
         (t3 (aref +crc32-slice+ 3))
         (t4 (aref +crc32-slice+ 4))
         (t5 (aref +crc32-slice+ 5))
         (t6 (aref +crc32-slice+ 6))
         (t7 (aref +crc32-slice+ 7))
         (t8 (aref +crc32-slice+ 8))
         (t9 (aref +crc32-slice+ 9))
         (t10 (aref +crc32-slice+ 10))
         (t11 (aref +crc32-slice+ 11))
         (t12 (aref +crc32-slice+ 12))
         (t13 (aref +crc32-slice+ 13))
         (t14 (aref +crc32-slice+ 14))
         (t15 (aref +crc32-slice+ 15)))
     (declare (type (simple-array (unsigned-byte 32) (*))
                    t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15))
     ,@body))

(defvar *pclmul-crc-impl* nil
  "Optional hardware CRC-32 implementation: a function of (CRC OCTETS START
END) returning (VALUES MID-CRC POS) after consuming a bulk prefix that is a
multiple of 64 bytes (POS varies), or NIL when unavailable.  Set by
pclmul-crc.lisp on SBCL/x86-64 little-endian machines whose CPU provides
PCLMULQDQ; the remainder always finishes through the slicer below.")

(defconstant +pclmul-threshold+ 64
  "Minimum input length for the PCLMULQDQ path (one fold iteration).  With a
nonzero initial CRC the 16-byte preamble runs first, so that case additionally
requires 80 bytes for the bulk loop to engage; shorter inputs slice.")

(defun %crc32-pclmul-prefix (crc octets start end)
  "Run the PCLMULQDQ bulk prefix when available; returns (VALUES CRC START).
One funcall per call, not per byte; the remainder slices below."
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum start end))
  (let ((impl *pclmul-crc-impl*))
    (when (and impl
               (if (zerop crc)
                   (>= (- end start) +pclmul-threshold+)
                   (>= (- end start) 80)))
      (multiple-value-bind (mid pos) (funcall impl crc octets start end)
        (setf crc mid
              start pos))))
  (values crc start))

(defmacro %crc32-load32 (octets i k)
  "Little-endian 32-bit word from OCTETS at I+K (portable byte assembly)."
  `(logior (aref ,octets (+ ,i ,k))
           (ash (aref ,octets (+ ,i ,k 1)) 8)
           (ash (aref ,octets (+ ,i ,k 2)) 16)
           (ash (aref ,octets (+ ,i ,k 3)) 24)))

#+(and sbcl cl-newzlib-le)
(defun crc32-update (crc octets start end)
  "Update CRC32 starting from CRC over OCTETS[START,END)."
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum start end)
           (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (crc start) (%crc32-pclmul-prefix crc octets start end)
    (let ((c (logxor crc #xFFFFFFFF)))
      (declare (type (unsigned-byte 32) c))
      ;; The SAP is taken once and the vector pinned for the whole loop, so
      ;; no allocation may occur inside (all unboxed word ops below).
      (with-crc32-tables
        (sb-sys:with-pinned-objects (octets)
          (let ((sap (sb-sys:vector-sap octets))
                (i start))
            (declare (type fixnum i))
            (loop while (>= (- end i) 16) do
              (let ((w0 (sb-sys:sap-ref-32 sap i))
                    (w1 (sb-sys:sap-ref-32 sap (+ i 4)))
                    (w2 (sb-sys:sap-ref-32 sap (+ i 8)))
                    (w3 (sb-sys:sap-ref-32 sap (+ i 12))))
                (declare (type (unsigned-byte 32) w0 w1 w2 w3))
                (setf c (%crc32-fold16 (logxor c w0) w1 w2 w3 t0 t1 t2 t3
                                       t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
                                       t14 t15)))
              (incf i 16))
            (loop while (>= (- end i) 8) do
              (let ((w0 (sb-sys:sap-ref-32 sap i))
                    (w1 (sb-sys:sap-ref-32 sap (+ i 4))))
                (declare (type (unsigned-byte 32) w0 w1))
                (setf c (%crc32-fold8 (logxor c w0) w1
                                      t0 t1 t2 t3 t4 t5 t6 t7)))
              (incf i 8))
            (setf c (%crc32-tail8 c octets i end t0)))))
      (logxor c #xFFFFFFFF))))

#+(and cl-newzlib-wide-fixnum (not (and sbcl cl-newzlib-le)))
(defun crc32-update (crc octets start end)
  "Update CRC32 starting from CRC over OCTETS[START,END).

Portable slicing-by-16 (same fold as the SBCL little-endian path, but
with the 32-bit words assembled from four byte loads so it runs anywhere
with wide fixnums).  Eight bytes fold through eight parallel lookups per
iteration instead of one lookup per byte."
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum start end)
           (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (crc start) (%crc32-pclmul-prefix crc octets start end)
    (let ((c (logxor crc #xFFFFFFFF)))
      (declare (type (unsigned-byte 32) c))
      (with-crc32-tables
        (let ((i start))
          (declare (type fixnum i))
          (loop while (>= (- end i) 16) do
            (let ((w0 (%crc32-load32 octets i 0))
                  (w1 (%crc32-load32 octets i 4))
                  (w2 (%crc32-load32 octets i 8))
                  (w3 (%crc32-load32 octets i 12)))
              (declare (type (unsigned-byte 32) w0 w1 w2 w3))
              (setf c (%crc32-fold16 (logxor c w0) w1 w2 w3 t0 t1 t2 t3
                                     t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
                                     t14 t15)))
            (incf i 16))
          ;; medium tail: one 8-byte fold with tables 0..7
          (loop while (>= (- end i) 8) do
            (let ((w0 (%crc32-load32 octets i 0))
                  (w1 (%crc32-load32 octets i 4)))
              (declare (type (unsigned-byte 32) w0 w1))
              (setf c (%crc32-fold8 (logxor c w0) w1
                                    t0 t1 t2 t3 t4 t5 t6 t7)))
            (incf i 8)
          (setf c (%crc32-tail8 c octets i end t0)))))
      (logxor c #xFFFFFFFF))))

#-(or (and sbcl cl-newzlib-le) cl-newzlib-wide-fixnum)
(defun crc32-update (crc octets start end)
  "Update CRC32 starting from CRC over OCTETS[START,END).
Portable path: the 32-bit working value is carried in two 16-bit words so
that no bignum boxing occurs on implementations whose fixnums are only 32
bits wide (Zach Beane's trick, also used by chipz)."
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum start end)
           (optimize (speed 3) (safety 0)))
  (let* ((table +crc32-table+)
         (high (ldb (byte 16 16) (logxor crc #xFFFFFFFF)))
         (low (ldb (byte 16 0) (logxor crc #xFFFFFFFF))))
    (declare (type (unsigned-byte 16) high low))
    (loop for i fixnum from start below end do
      (let ((index (logxor (logand low #xFF) (aref octets i))))
        (declare (type (integer 0 255) index))
        ;; NB: named ENTRY, not T -- T is a constant and must not be bound
        ;; (SBCL tolerates it, stricter implementations signal an error).
        (let ((entry (aref table index)))
          (declare (type (unsigned-byte 32) entry))
          (setf low (logxor (ash (logand high #xFF) 8)
                            (ash low -8)
                            (logand entry #xFFFF))
                high (logxor (ash high -8) (ash entry -16))))))
    (logxor (logior (ash high 16) low) #xFFFFFFFF)))

(defun crc32 (octets &optional (start 0) (end (length octets))
                     (initial-crc 0))
  "Return the CRC-32 of OCTETS[START,END) given INITIAL-CRC."
  (crc32-update initial-crc octets start end))
