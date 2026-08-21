(in-package #:cl-newzlib)

;;; Optional SIMD acceleration (SBCL on x86-64, via the bundled sb-simd
;;; contrib).  Everything here degrades gracefully:
;;;
;;;   * other Lisps / architectures get the plain scalar definitions,
;;;   * SBCL builds whose contrib is missing get a scalar definition,
;;;   * x86-64 CPUs without AVX2 take the SSE2 path (baseline on x86-64),
;;;     selected at image startup by sb-simd's INSTRUCTION-SET-CASE.
;;;
;;; The vector loads go through sb-simd's AREF interface, which re-derives
;;; the data vector on every access and is therefore GC-safe without any
;;; manual pinning.

#+(and sbcl x86-64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case (require :sb-simd) (error () nil))
  (when (find-package :sb-simd-avx2)
    (pushnew :newzlib-simd *features*)))

;;; ------------------------------------------------------------------
;;; Leading-equal-octets: how many bytes of INPUT[A..] and INPUT[B..]
;;; agree, counting up to LIMIT.  Used by the deflate match extender.
;;; ------------------------------------------------------------------

#-newzlib-simd
(progn
  (declaim (inline leading-equal-octets))
  (defun leading-equal-octets (input a b limit)
    (declare (optimize (speed 3) (safety 0))
             (type (simple-array (unsigned-byte 8) (*)) input)
             (type fixnum a b limit))
    (let ((n 0))
      (declare (type fixnum n))
      (loop while (and (< n limit)
                       (= (aref input (+ a n)) (aref input (+ b n))))
            do (incf n))
      n)))

#+newzlib-simd
(progn
  (defun %leading-equal-octets/avx2 (input a b limit)
    (declare (optimize (speed 3) (safety 0))
             (type (simple-array (unsigned-byte 8) (*)) input)
             (type fixnum a b limit))
    (let ((n 0))
      (declare (type fixnum n))
      (loop while (>= (- limit n) 32)
            do (let* ((ia (+ a n))
                      (ib (+ b n))
                      (mask (sb-simd-avx2:u8.32-movemask
                             (sb-simd-avx2:u8.32=
                              (sb-simd-avx2:u8.32-aref input ia)
                              (sb-simd-avx2:u8.32-aref input ib)))))
                 (declare (type (unsigned-byte 32) mask))
                 ;; MOVEMASK yields an unsigned value: all lanes equal is
                 ;; #xFFFFFFFF, not -1
                 (cond ((= mask #xFFFFFFFF)
                        (incf n 32))
                       (t
                        ;; lowest zero bit of the equality mask = first
                        ;; mismatching byte
                        (incf n (1- (logcount (logxor mask
                                                      (ldb (byte 32 0)
                                                           (1+ mask))))))
                        (return-from %leading-equal-octets/avx2 n)))))
      ;; scalar tail below one vector width
      (loop while (and (< n limit)
                       (= (aref input (+ a n)) (aref input (+ b n))))
            do (incf n))
      n))

  (declaim (inline leading-equal-octets/scalar))
  (defun leading-equal-octets/scalar (input a b limit)
    (declare (optimize (speed 3) (safety 0))
             (type (simple-array (unsigned-byte 8) (*)) input)
             (type fixnum a b limit))
    (let ((n 0))
      (declare (type fixnum n))
      (loop while (and (< n limit)
                       (= (aref input (+ a n)) (aref input (+ b n))))
            do (incf n))
      n))

  (defun %leading-equal-octets/sse2 (input a b limit)
    (declare (optimize (speed 3) (safety 0))
             (type (simple-array (unsigned-byte 8) (*)) input)
             (type fixnum a b limit))
    (let ((n 0))
      (declare (type fixnum n))
      (loop while (>= (- limit n) 16)
            do (let* ((ia (+ a n))
                      (ib (+ b n))
                      (mask (sb-simd-sse4.1:u8.16-movemask
                             (sb-simd-sse4.1:u8.16=
                              (sb-simd-sse4.1:u8.16-aref input ia)
                              (sb-simd-sse4.1:u8.16-aref input ib)))))
                 (declare (type (unsigned-byte 16) mask))
                 (cond ((= mask #xFFFF)
                        (incf n 16))
                       (t
                        (incf n (1- (logcount (logxor mask
                                                      (ldb (byte 16 0)
                                                           (1+ mask))))))
                        (return-from %leading-equal-octets/sse2 n)))))
      (loop while (and (< n limit)
                       (= (aref input (+ a n)) (aref input (+ b n))))
            do (incf n))
      n))

  (declaim (inline leading-equal-octets))
  (defun leading-equal-octets (input a b limit)
    (declare (optimize (speed 3) (safety 0))
             (type (simple-array (unsigned-byte 8) (*)) input)
             (type fixnum a b limit))
    (sb-simd-internals:instruction-set-case
      (:avx2 (%leading-equal-octets/avx2 input a b limit))
      (:sse4.1 (%leading-equal-octets/sse2 input a b limit))
      (:x86-64 (leading-equal-octets/scalar input a b limit)))))