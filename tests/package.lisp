(defpackage #:cl-newzlib-tests
  (:use #:cl #:cl-newzlib #:fiveam)
  (:export #:run-tests))

(in-package #:cl-newzlib-tests)

(def-suite cl-newzlib-suite
  :description "Test suite for cl-newzlib.")

(in-suite cl-newzlib-suite)

;;; Deterministic PRNG so tests are reproducible without external deps.
(defparameter *prng-state* 12345)

(defun next-random-octet ()
  (setf *prng-state* (logand (+ (* *prng-state* 1103515245) 12345) #x7fffffff))
  (logand (ash *prng-state* -16) #xFF))

(defun random-octets (n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (next-random-octet)))))

(defun run-tests ()
  "Run the full cl-newzlib test suite and return the FiveAM result."
  (let ((result (run 'cl-newzlib-suite)))
    (explain! result)
    result))
