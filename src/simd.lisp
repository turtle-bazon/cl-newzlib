(in-package #:cl-newzlib)

;;; Optional SIMD acceleration (SBCL on x86-64, via the bundled sb-simd
;;; contrib).  Everything here degrades gracefully:
;;;
;;;   * other Lisps / architectures get the plain scalar definitions,
;;;   * SBCL builds whose contrib is missing get a scalar definition,
;;;   * x86-64 CPUs without AVX2 take the SSE4.1 or scalar path, selected at
;;;     runtime by SB-CPU-FEATURES (and re-selected after image restart).
;;;
;;; The feature gate :newzlib-simd is pushed in util.lisp (loaded earlier).
;;; The vector loads go through sb-simd's AREF interface, which re-derives
;;; the data vector on every access and is therefore GC-safe without any
;;; manual pinning.

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
  #+newzlib-avx2
  (defun %leading-equal-octets/avx2 (input a b limit)
    (declare (optimize (speed 3) (safety 0))
             (type (simple-array (unsigned-byte 8) (*)) input)
             (type fixnum a b limit))
    #-newzlib-avx2-build
    (declare (notinline sb-simd-avx2:u8.32-movemask
                        sb-simd-avx2:u8.32=
                        sb-simd-avx:u8.32-aref))
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

  ;; Selection goes through SB-CPU-FEATURES, so the runtime check and the
  ;; masking macros match the rest of the library.  AVX2 is gated on both
  ;; CPUID and OS XMM/YMM state; SSE4.1 is gated on its CPUID bit.  Called
  ;; once per compression, so image migration and restart re-evaluate the
  ;; current machine.
  (sb-cpu-features:define-cpu-dispatch select-leading-equal-octets
    #+newzlib-avx2
    (:avx2 %leading-equal-octets/avx2)
    (:sse4.1 %leading-equal-octets/sse2)
    (:default leading-equal-octets/scalar))

  (declaim (inline leading-equal-octets))
  (defun leading-equal-octets (input a b limit)
    (declare (optimize (speed 3) (safety 0))
             (type (simple-array (unsigned-byte 8) (*)) input)
             (type fixnum a b limit))
    (the fixnum
      (funcall (select-leading-equal-octets) input a b limit))))

#-newzlib-simd
(defun select-leading-equal-octets ()
  #'leading-equal-octets)

(declaim (ftype (function ((simple-array (unsigned-byte 8) (*))
                           fixnum fixnum fixnum)
                          fixnum)
                 leading-equal-octets))
