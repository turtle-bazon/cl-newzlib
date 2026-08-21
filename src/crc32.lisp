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

#+sbcl
(defun crc32-update (crc octets start end)
  "Update CRC32 starting from CRC over OCTETS[START,END)."
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum start end)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((table +crc32-table+)
        (c (logxor crc #xFFFFFFFF)))
    (declare (type (unsigned-byte 32) c))
    (loop for i fixnum from start below end do
      (setf c (logxor (aref table (logand #xFF (logxor c (aref octets i))))
                      (ash c -8))))
    (logxor c #xFFFFFFFF)))

#-sbcl
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
        (let ((t (aref table index)))
          (declare (type (unsigned-byte 32) t))
          (setf low (logxor (ash (logand high #xFF) 8)
                            (ash low -8)
                            (logand t #xFFFF))
                high (logxor (ash high -8) (ash t -16))))))
    (logxor (logior (ash high 16) low) #xFFFFFFFF)))

(defun crc32 (octets &optional (start 0) (end (length octets))
                     (initial-crc 0))
  "Return the CRC-32 of OCTETS[START,END) given INITIAL-CRC."
  (crc32-update initial-crc octets start end))
