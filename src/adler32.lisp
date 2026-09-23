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

(defvar *simd-adler-impl* nil
  "Optional vectorized Adler-32: a function of (OCTETS START END
INITIAL-ADLER) returning the checksum, or NIL when unavailable.  Set by
simd-adler.lisp on SBCL/x86-64 machines with AVX2 (after a load-time
self-test); shorter inputs and the <32-byte tail always use %adler32.")

(defconstant +simd-adler-threshold+ 1024
  "Minimum input length for the vector path (setup costs dominate below).")

;;; Fold both sums modulo +adler-mod+ at least every +adler-chunk+ bytes.
;;; 2048 keeps the worst-case 32-bit growth of the eight-way-unrolled loop
;;; (s2 gains at most ~8*s1 per group) safely below 2^32, mirroring zlib's
;;; NMAX analysis.
(defconstant +adler-chunk+ 2048)

(declaim (inline %adler32 %adler-tail))

(defun %adler-tail (octets i end s1 s2)
  "Scalar tail to END; returns (VALUES S1 S2)."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum i end)
           (type (unsigned-byte 32) s1 s2)
           (optimize (speed 3) (safety 0) (debug 0)))
  (loop while (< i end) do
    (setf s1 (+ s1 (aref octets i))
          s2 (+ s2 s1)
          i (1+ i)))
  (values s1 s2))

(defun %adler32 (octets start end s1 s2)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum start end) (type (unsigned-byte 32) s1 s2)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((i start))
    (declare (type fixnum i))
    (loop while (< i end) do
      (setf s1 (mod s1 +adler-mod+)
            s2 (mod s2 +adler-mod+))
      (let ((chunk-end (min end (+ i +adler-chunk+))))
        (declare (type fixnum chunk-end))
        ;; eight independent loads feed two short chains the CPU overlaps
        (let ((block-end (- chunk-end (mod (- chunk-end i) 8))))
          (declare (type fixnum block-end))
          (loop while (< i block-end) do
            (let ((b0 (aref octets i))
                  (b1 (aref octets (+ i 1)))
                  (b2 (aref octets (+ i 2)))
                  (b3 (aref octets (+ i 3)))
                  (b4 (aref octets (+ i 4)))
                  (b5 (aref octets (+ i 5)))
                  (b6 (aref octets (+ i 6)))
                  (b7 (aref octets (+ i 7))))
              (declare (type (unsigned-byte 32) b0 b1 b2 b3 b4 b5 b6 b7)
                       (type fixnum b0 b1 b2 b3 b4 b5 b6 b7))
              (setf s1 (+ s1 b0) s2 (+ s2 s1)
                    s1 (+ s1 b1) s2 (+ s2 s1)
                    s1 (+ s1 b2) s2 (+ s2 s1)
                    s1 (+ s1 b3) s2 (+ s2 s1)
                    s1 (+ s1 b4) s2 (+ s2 s1)
                    s1 (+ s1 b5) s2 (+ s2 s1)
                    s1 (+ s1 b6) s2 (+ s2 s1)
                    s1 (+ s1 b7) s2 (+ s2 s1)
                    i (+ i 8)))))
        ;; tail bytes of this chunk
        (multiple-value-bind (ns1 ns2) (%adler-tail octets i chunk-end s1 s2)
          (setf s1 ns1 s2 ns2 i chunk-end))))
    (setf s1 (mod s1 +adler-mod+)
          s2 (mod s2 +adler-mod+))
    (logior (ash s2 16) s1)))

(defun adler32 (octets &optional (start 0) (end (length octets))
                        (initial-adler 1))
  "Return the Adler-32 checksum of OCTETS[START,END) given INITIAL-ADLER."
  (declare (type (unsigned-byte 32) initial-adler)
           (optimize (speed 3) (safety 0)))
  ;; Vector fast path (one funcall per call, not per byte).
  (let ((impl *simd-adler-impl*))
    (when (and impl (>= (- end start) +simd-adler-threshold+))
      (return-from adler32 (funcall impl octets start end initial-adler))))
  (%adler32 octets start end
            (ldb (byte 16 0) initial-adler)
            (ldb (byte 16 16) initial-adler)))
