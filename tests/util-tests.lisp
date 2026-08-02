(in-package #:cl-newzlib-tests)

(in-suite cl-newzlib-suite)

(test version-string
  (is (stringp (cl-newzlib:version))))

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
