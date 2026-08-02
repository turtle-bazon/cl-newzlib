(in-package #:cl-newzlib-tests)

(in-suite cl-newzlib-suite)

(test write-single-byte
  (let ((w (cl-newzlib::make-bit-writer 8)))
    (cl-newzlib::write-bits w #b10101100 8)
    (cl-newzlib::flush-bits w)
    (let ((bytes (cl-newzlib::writer-bytes w)))
      (is (= 1 (length bytes)))
      (is (= #b10101100 (aref bytes 0))))))

(test bit-roundtrip
  (let* ((w (cl-newzlib::make-bit-writer 8))
         (seq '((#b1101 . 4)
                (#b0 . 1)
                (#b1010101010101010 . 16)
                (#b111 . 3)
                (#b01 . 2)))
         (bytes (progn
                  (dolist (p seq) (cl-newzlib::write-bits w (car p) (cdr p)))
                  (cl-newzlib::flush-bits w)
                  (cl-newzlib::writer-bytes w)))
         (r (cl-newzlib::make-bit-reader bytes)))
    (dolist (p seq)
      (is (= (car p) (cl-newzlib::read-bits r (cdr p)))))))

(test peek-does-not-consume
  (let* ((w (cl-newzlib::make-bit-writer 8))
         (bytes (progn
                  (cl-newzlib::write-bits w #b1110 4)
                  (cl-newzlib::flush-bits w)
                  (cl-newzlib::writer-bytes w)))
         (r (cl-newzlib::make-bit-reader bytes)))
    (is (= #b1110 (cl-newzlib::peek-bits r 4)))
    (is (= #b1110 (cl-newzlib::peek-bits r 4)))
    (is (= #b1110 (cl-newzlib::read-bits r 4)))
    (is (= 0 (cl-newzlib::peek-bits r 4)))))

(test cross-byte-boundary
  (let* ((w (cl-newzlib::make-bit-writer 2))
         (seq '((#b1 . 1) (#b0 . 1) (#b11100110 . 8) (#b101 . 3)))
         (bytes (progn
                  (dolist (p seq) (cl-newzlib::write-bits w (car p) (cdr p)))
                  (cl-newzlib::flush-bits w)
                  (cl-newzlib::writer-bytes w)))
         (r (cl-newzlib::make-bit-reader bytes)))
    (dolist (p seq)
      (is (= (car p) (cl-newzlib::read-bits r (cdr p)))))))

(test writer-grows
  (let* ((w (cl-newzlib::make-bit-writer 2))
         (n 1000)
         (bytes (progn
                  (dotimes (i n)
                    (cl-newzlib::write-bits w (logand i #xFF) 8))
                  (cl-newzlib::flush-bits w)
                  (cl-newzlib::writer-bytes w)))
         (r (cl-newzlib::make-bit-reader bytes)))
    (is (= n (length bytes)))
    (dotimes (i n)
      (is (= (logand i #xFF) (cl-newzlib::read-bits r 8))))))

(test bit-reader-end-of-input
  (let* ((w (cl-newzlib::make-bit-writer 8))
         (bytes (progn
                  (cl-newzlib::write-bits w 5 8)
                  (cl-newzlib::flush-bits w)
                  (cl-newzlib::writer-bytes w)))
         (r (cl-newzlib::make-bit-reader bytes)))
    (cl-newzlib::read-bits r 8)
    (signals cl-newzlib:newzlib-end-of-input
      (cl-newzlib::read-bits r 8))))
