(in-package #:cl-newzlib-tests)

(in-suite cl-newzlib-suite)

;;; build-huffman-codes uses FREQ beyond ELEMS as scratch for internal nodes,
;;; so always hand it a generously sized frequency vector.
(defparameter +test-freq-size+ 600)

(test length-code-table
  ;; 0-based codes (DEFLATE code = 257 + result)
  (is (= 0 (cl-newzlib::length-code 3)))
  (is (= 1 (cl-newzlib::length-code 4)))
  (is (= 5 (cl-newzlib::length-code 8)))
  (is (= 6 (cl-newzlib::length-code 9)))
  (is (= 12 (cl-newzlib::length-code 19)))
  (is (= 16 (cl-newzlib::length-code 35)))
  (is (= 20 (cl-newzlib::length-code 67)))
  (is (= 24 (cl-newzlib::length-code 131)))
  (is (= 28 (cl-newzlib::length-code 258))))

(test length-extra-bits
  (is (= 0 (cl-newzlib::length-extra-bits 0)))
  (is (= 1 (cl-newzlib::length-extra-bits 8)))
  (is (= 3 (cl-newzlib::length-extra-bits 16)))
  (is (= 0 (cl-newzlib::length-extra-bits 28))))

(test length-base
  (is (= 3 (cl-newzlib::length-base 0)))
  (is (= 11 (cl-newzlib::length-base 8)))
  (is (= 35 (cl-newzlib::length-base 16)))
  (is (= 258 (cl-newzlib::length-base 28))))

(test dist-code-table
  (is (= 0 (cl-newzlib::dist-code 1)))
  (is (= 1 (cl-newzlib::dist-code 2)))
  (is (= 2 (cl-newzlib::dist-code 3)))
  (is (= 3 (cl-newzlib::dist-code 4)))
  (is (= 9 (cl-newzlib::dist-code 32)))
  (is (= 10 (cl-newzlib::dist-code 33)))
  (is (= 17 (cl-newzlib::dist-code 512)))
  (is (= 19 (cl-newzlib::dist-code 1024)))
  (is (= 27 (cl-newzlib::dist-code 16384)))
  (is (= 29 (cl-newzlib::dist-code 32768))))

(test dist-extra-bits
  (is (= 0 (cl-newzlib::dist-extra-bits 0)))
  (is (= 1 (cl-newzlib::dist-extra-bits 4)))
  (is (= 6 (cl-newzlib::dist-extra-bits 14)))
  (is (= 13 (cl-newzlib::dist-extra-bits 29))))

(test dist-base
  (is (= 1 (cl-newzlib::dist-base 0)))
  (is (= 5 (cl-newzlib::dist-base 4)))
  (is (= 17 (cl-newzlib::dist-base 8)))
  (is (= 24577 (cl-newzlib::dist-base 29))))

(test build-huffman-codes-canonical
  (let* ((freq (make-array +test-freq-size+ :element-type 'fixnum :initial-element 0))
         (elems 6))
    (setf (aref freq 0) 10 (aref freq 1) 1 (aref freq 2) 15
          (aref freq 3) 3 (aref freq 4) 8 (aref freq 5) 2)
    (multiple-value-bind (lengths codes max-code)
        (cl-newzlib::build-huffman-codes freq elems 7)
      ;; no code longer than the limit
      (is (every (lambda (l) (<= 0 l 7)) lengths))
      ;; every symbol with a nonzero frequency got a code length
      (loop for i below elems
            do (is (plusp (aref lengths i))))
      ;; Kraft inequality over used symbols: sum(2^-len) <= 1
      (is (<= (loop for l across lengths when (plusp l) sum (expt 2 (- l)))
              1.000001))
      ;; codes are canonical: 0 <= code < 2^len
      (loop for i below elems
            do (is (< (aref codes i) (expt 2 (aref lengths i)))))
      (is (plusp max-code)))))

(test build-huffman-codes-empty
  (let ((freq (make-array +test-freq-size+ :element-type 'fixnum :initial-element 0)))
    (multiple-value-bind (lengths codes max-code)
        (cl-newzlib::build-huffman-codes freq 3 7)
      ;; pkzip forces at least two 1-bit codes even for empty input
      (is (plusp max-code))
      (is (plusp (aref lengths 0)))
      (is (plusp (aref lengths 1)))
      (is (= 0 (aref lengths 2)))
      (loop for i below 3
            do (is (< (aref codes i) (expt 2 (aref lengths i))))))))

(test decode-table-kraft-reject
  ;; code lengths violating Kraft's inequality must be rejected
  (let ((bad (make-array 3 :element-type 'fixnum :initial-element 0)))
    (setf (aref bad 0) 1 (aref bad 1) 1 (aref bad 2) 1)
    (signals cl-newzlib:newzlib-format-error
      (cl-newzlib::build-huffman-decode-table bad))))

(test decode-table-roundtrip
  (let* ((freq (make-array +test-freq-size+ :element-type 'fixnum :initial-element 0))
         (symbols '(4 1 3 0 2)))          ; symbol *indices*, 0..4
    (setf (aref freq 0) 5 (aref freq 1) 5 (aref freq 2) 5
          (aref freq 3) 1 (aref freq 4) 1)
    (multiple-value-bind (lens codes)
        (cl-newzlib::build-huffman-codes freq 5 7)
      (let ((table (cl-newzlib::build-huffman-decode-table lens 0 5))
            (w (cl-newzlib::make-bit-writer 16)))
        (dolist (s symbols)
          (cl-newzlib::write-bits w (aref codes s) (aref lens s)))
        (cl-newzlib::flush-bits w)
        (let ((r (cl-newzlib::make-bit-reader (cl-newzlib::writer-bytes w))))
          (dolist (s symbols)
            (is (= s (cl-newzlib::huffman-decode table r)))))))))

(test static-trees
  (cl-newzlib::ensure-static-trees)
  (multiple-value-bind (codes lengths)
      (cl-newzlib::compute-static-lit-tree)
    (is (= 288 (length codes)))
    (is (= 8 (aref lengths 0)))
    (is (= 8 (aref lengths 143)))
    (is (= 9 (aref lengths 144)))
    (is (= 7 (aref lengths 256)))
    (is (= 8 (aref lengths 280)))
    (is (= 8 (aref lengths 287)))
    (is (every (lambda (l) (<= 1 l 9)) lengths))))
