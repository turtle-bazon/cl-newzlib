(in-package #:cl-newzlib-tests)

(in-suite cl-newzlib-suite)

(test version-string
  (is (stringp (cl-newzlib:version))))

#+sbcl
(test cpu-feature-layer-wired-in
  (is (find-package :sb-cpu-features))
  (is (member 'cl-newzlib::refresh-cpu-dependent-implementations
              sb-ext:*init-hooks*)))

#+(and sbcl x86-64)
(test avx2-usable-p-uses-sb-cpu-features
  (is (eq (not (null (cl-newzlib::avx2-usable-p)))
          (not (null (sb-cpu-features:cpu-feature-p :avx2)))))
  (sb-cpu-features:with-cpu-features-only '()
    (is (not (cl-newzlib::avx2-usable-p)))))

#+sbcl
(test cpu-init-hook-refreshes-features
  (let ((sb-cpu-features::*detected-cpu-features* '(:bogus))
        (sb-cpu-features::*cpu-features-probed-p* t))
    (cl-newzlib::refresh-cpu-dependent-implementations)
    (is (not (member :bogus (sb-cpu-features:cpu-features))))))

(test level-constants
  (is (= 0 cl-newzlib:+no-compression+))
  (is (= 1 cl-newzlib:compression-level-fastest))
  (is (= 6 cl-newzlib:+default-compression+))
  (is (= 9 cl-newzlib:+best-compression+)))

(test check-compression-level-valid
  (loop for level in '(0 1 2 3 4 5 6 7 8 9)
        do (is (= level (cl-newzlib::check-compression-level level)))))

(test check-compression-level-invalid
  (signals cl-newzlib:newzlib-parameter-error
    (cl-newzlib::check-compression-level -1))
  (signals cl-newzlib:newzlib-parameter-error
    (cl-newzlib::check-compression-level 10))
  (signals cl-newzlib:newzlib-parameter-error
    (cl-newzlib::check-compression-level :high)))

(test check-compression-mode
  (is (eq :standard (cl-newzlib::check-compression-mode :standard)))
  (is (eq :fast (cl-newzlib::check-compression-mode :fast)))
  (signals cl-newzlib:newzlib-parameter-error
    (cl-newzlib::check-compression-mode :bogus)))

(test ubyte8-ref-set
  (let ((v (cl-newzlib::make-octet-buffer 4)))
    (setf (aref v 0) 10)
    (cl-newzlib::ubyte8-set v 1 255)
    (cl-newzlib::ubyte8-set v 2 #xAA)
    (is (= 10 (cl-newzlib::ubyte8-ref v 0)))
    (is (= 255 (cl-newzlib::ubyte8-ref v 1)))
    (is (= #xAA (cl-newzlib::ubyte8-ref v 2)))
    (is (= 0 (cl-newzlib::ubyte8-ref v 3)))))

(test octets-copy
  (let ((src (cl-newzlib::make-octet-buffer 6))
        (dst (cl-newzlib::make-octet-buffer 6)))
    (dotimes (i 6) (setf (aref src i) i))
    (cl-newzlib::octets-copy src 1 dst 2 3)
    (is (= 0 (aref dst 0)))
    (is (= 0 (aref dst 1)))
    (is (= 1 (aref dst 2)))
    (is (= 2 (aref dst 3)))
    (is (= 3 (aref dst 4)))
    (is (= 0 (aref dst 5)))))

(test growable-buffer
  (let ((g (cl-newzlib::make-growable-buffer 2)))
    (is (= 0 (length g)))
    (loop for i below 100 do (vector-push-extend (logand i #xFF) g))
    (is (= 100 (length g)))
    (is (= 99 (aref g 99)))))

(test adler32-known-values
  ;; zlib's own test values: adler32("Wikipedia") = 0x11E60398
  (let ((v (map '(vector (unsigned-byte 8)) #'char-code "Wikipedia")))
    (is (= #x11E60398 (cl-newzlib:adler32 v))))
  (is (= 1 (cl-newzlib:adler32 #())))
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
    (is (= 1 (cl-newzlib:adler32 empty)))))

(test adler32-partial
  (let* ((a (random-octets 64))
         (b (random-octets 64))
         (ab (concatenate '(vector (unsigned-byte 8)) a b))
         (mid (length a)))
    (is (= (cl-newzlib:adler32 ab)
           (cl-newzlib:adler32 ab mid (length ab)
                               (cl-newzlib:adler32 ab 0 mid))))))

(test crc32-known-values
  ;; crc32("123456789") = 0xCBF43926
  (let ((v (map '(vector (unsigned-byte 8)) #'char-code "123456789")))
    (is (= #xCBF43926 (cl-newzlib:crc32 v))))
  (is (= 0 (cl-newzlib:crc32 (make-array 0 :element-type '(unsigned-byte 8))))))

(test crc32-incremental
  (let* ((data (random-octets 128))
         (whole (cl-newzlib:crc32 data))
         (part1 (cl-newzlib:crc32 data 0 64))
         (part2 (cl-newzlib:crc32 data 64 128 part1)))
    (is (= whole part2))))

#+newzlib-simd
(test simd-leading-equal-dispatch
  ;; The selected implementation must agree with the scalar oracle; this also
  ;; exercises the runtime instruction-set dispatch used for image migration.
  (let ((data (random-octets 512))
        (fn (cl-newzlib::select-leading-equal-octets)))
    (dotimes (i 64)
      (let* ((a (mod (* i 37) 480))
             (b (mod (* i 53) 480))
             (limit (min (- 512 (max a b)) (+ 1 (mod i 31)))))
        (is (= (cl-newzlib::leading-equal-octets/scalar data a b limit)
               (funcall fn data a b limit)))))))

#+(and sbcl newzlib-avx2)
(test simd-adler-reselects-at-runtime
  ;; A compiled AVX2 image must clear the implementation when the capability
  ;; check fails, and restore it when the CPU is capable again.  Only the
  ;; re-enable half runs on an actually capable CPU (otherwise it would try to
  ;; execute AVX2 here).
  (let ((predicate #'cl-newzlib::simd-adler-cpu-available-p)
        (capable (cl-newzlib::simd-adler-cpu-available-p))
        (impl cl-newzlib::*simd-adler-impl*))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-newzlib::simd-adler-cpu-available-p)
                 (lambda () nil))
           (cl-newzlib::update-simd-adler-impl)
           (is (null cl-newzlib::*simd-adler-impl*))
           (when capable
             (setf (symbol-function 'cl-newzlib::simd-adler-cpu-available-p)
                   predicate)
             (cl-newzlib::update-simd-adler-impl)
             (is (functionp cl-newzlib::*simd-adler-impl*))))
      (setf (symbol-function 'cl-newzlib::simd-adler-cpu-available-p) predicate
            cl-newzlib::*simd-adler-impl* impl))))

#+(and sbcl newzlib-pclmul)
(test pclmul-crc-reselects-at-runtime
  (let ((predicate #'cl-newzlib::pclmul-cpu-available-p)
        (capable (cl-newzlib::pclmul-cpu-available-p))
        (impl cl-newzlib::*pclmul-crc-impl*))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-newzlib::pclmul-cpu-available-p)
                 (lambda () nil))
           (cl-newzlib::update-pclmul-crc-impl)
           (is (null cl-newzlib::*pclmul-crc-impl*))
           (when capable
             (setf (symbol-function 'cl-newzlib::pclmul-cpu-available-p)
                   predicate)
             (cl-newzlib::update-pclmul-crc-impl)
             (is (functionp cl-newzlib::*pclmul-crc-impl*))))
      (setf (symbol-function 'cl-newzlib::pclmul-cpu-available-p) predicate
            cl-newzlib::*pclmul-crc-impl* impl))))

#+(and sbcl newzlib-avx2)
(test avx2-os-state-gate
  ;; CPUID can advertise AVX2 while the OS has not enabled YMM state.
  ;; Masking :AVX2 makes the shared feature layer report it unavailable,
  ;; so the runtime selector must avoid the AVX2 implementation.
  (sb-cpu-features:with-cpu-features-disabled (:avx2)
    (is (not (eq (cl-newzlib::select-leading-equal-octets)
                 #'cl-newzlib::%leading-equal-octets/avx2)))
    (cl-newzlib::update-simd-adler-impl)
    (is (null cl-newzlib::*simd-adler-impl*)))
  (cl-newzlib::update-simd-adler-impl))

#+(and sbcl newzlib-simd)
(test sse41-dispatch-uses-cpu-features
  ;; With every feature masked, the matcher must fall back to scalar.
  (sb-cpu-features:with-cpu-features-only '()
    (is (eq (cl-newzlib::select-leading-equal-octets)
            #'cl-newzlib::leading-equal-octets/scalar))))

#+(and sbcl newzlib-pclmul)
(test pclmul-availability-uses-cpu-features
  (is (eq (not (null (cl-newzlib::pclmul-cpu-available-p)))
          (not (null (sb-cpu-features:cpu-feature-p :pclmulqdq)))))
  (sb-cpu-features:with-cpu-features-disabled (:pclmulqdq)
    (is (not (cl-newzlib::pclmul-cpu-available-p)))))
