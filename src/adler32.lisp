(in-package #:cl-newzlib)

;;; RFC 1950 Adler-32 checksum.
;;;
;;;   A = 1 + sum of bytes
;;;   B = sum of prefix sums
;;;   adler32 = (B << 16) | A   (mod 65521)
;;;
;;; The bytes are consumed in blocks of ADLER-CHUNK and both sums are folded
;;; modulo 65521 at each block boundary, which keeps the values inside fixnum
;;; range on 32-bit implementations while avoiding a mod per byte.

(defconstant +adler-mod+ 65521)
(defconstant +adler-chunk+ 16)

(declaim (inline %adler32))
(defun %adler32 (octets start end s1 s2)
  (declare (type simple-array octets)
           (type fixnum start end)
           (type (unsigned-byte 32) s1 s2)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((i start)
        (length (- end start)))
    (declare (type fixnum i length))
    (loop while (plusp length) do
      (let ((k (min +adler-chunk+ length)))
        (declare (type fixnum k))
        (setf length (- length k))
        (loop while (plusp k) do
          (setf s1 (+ s1 (aref octets i))
                s2 (+ s2 s1)
                i (1+ i)
                k (1- k)))
        (setf s1 (mod s1 +adler-mod+)
              s2 (mod s2 +adler-mod+))))
    (logior (ash s2 16) s1)))

(defun adler32 (octets &optional (start 0) (end (length octets))
                        (initial-adler 1))
  "Return the Adler-32 checksum of OCTETS[START,END) given INITIAL-ADLER."
  (declare (type (unsigned-byte 32) initial-adler)
           (optimize (speed 3) (safety 0)))
  (%adler32 octets start end
            (ldb (byte 16 0) initial-adler)
            (ldb (byte 16 16) initial-adler)))
