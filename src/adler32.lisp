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
;;; Fold both sums modulo +adler-mod+ at least every +adler-chunk+ bytes.
;;; 4096 keeps the worst-case 32-bit growth of the four-way-unrolled loop
;;; (s2 gains at most ~4*s1 per block, s1 at most 1020 per block) far below
;;; 2^32, mirroring zlib's NMAX analysis.
(defconstant +adler-chunk+ 4096)

(declaim (inline %adler32))
(defun %adler32 (octets start end s1 s2)
  (declare (type simple-array octets)
           (type fixnum start end)
           (type (unsigned-byte 32) s1 s2)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((i start))
    (declare (type fixnum i))
    (loop while (< i end) do
      (setf s1 (mod s1 +adler-mod+)
            s2 (mod s2 +adler-mod+))
      (let ((chunk-end (min end (+ i +adler-chunk+))))
        (declare (type fixnum chunk-end))
        ;; four bytes at a time: the two sums are updated with independent
        ;; expressions, breaking the serial dependency of the naive loop
        (let ((block-end (- chunk-end (mod (- chunk-end i) 4))))
          (declare (type fixnum block-end))
          (loop while (< i block-end) do
            (let* ((b0 (aref octets i))
                   (b1 (aref octets (+ i 1)))
                   (b2 (aref octets (+ i 2)))
                   (b3 (aref octets (+ i 3)))
                   (s1-0 s1))
              (declare (type (unsigned-byte 32) b0 b1 b2 b3 s1-0)
                       (type fixnum b0 b1 b2 b3))
              (setf s2 (+ s2 (* 4 s1-0) (* 4 b0) (* 3 b1) (* 2 b2) b3)
                    s1 (+ s1-0 b0 b1 b2 b3)
                    i (+ i 4)))))
        ;; tail bytes
        (loop while (< i chunk-end) do
          (setf s1 (+ s1 (aref octets i))
                s2 (+ s2 s1)
                i (1+ i)))))
    (setf s1 (mod s1 +adler-mod+)
          s2 (mod s2 +adler-mod+))
    (logior (ash s2 16) s1)))

(defun adler32 (octets &optional (start 0) (end (length octets))
                        (initial-adler 1))
  "Return the Adler-32 checksum of OCTETS[START,END) given INITIAL-ADLER."
  (declare (type (unsigned-byte 32) initial-adler)
           (optimize (speed 3) (safety 0)))
  (%adler32 octets start end
            (ldb (byte 16 0) initial-adler)
            (ldb (byte 16 16) initial-adler)))
