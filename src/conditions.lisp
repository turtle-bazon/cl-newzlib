(in-package #:cl-newzlib)

(define-condition newzlib-error (error)
  ()
  (:documentation "Base class for all cl-newzlib errors."))

(define-condition newzlib-parameter-error (newzlib-error)
  ((detail :initarg :detail :initform nil :reader newzlib-error-detail))
  (:report (lambda (condition stream)
             (format stream "cl-newzlib: invalid parameter~@[: ~A~]"
                     (newzlib-error-detail condition)))))

(define-condition newzlib-format-error (newzlib-error)
  ((detail :initarg :detail :initform nil :reader newzlib-error-detail))
  (:report (lambda (condition stream)
             (format stream "cl-newzlib: corrupt or unsupported input data~@[: ~A~]"
                     (newzlib-error-detail condition)))))

(define-condition newzlib-unsupported-error (newzlib-error)
  ((detail :initarg :detail :initform nil :reader newzlib-error-detail))
  (:report (lambda (condition stream)
             (format stream "cl-newzlib: unsupported feature~@[: ~A~]"
                     (newzlib-error-detail condition)))))

(define-condition newzlib-memory-error (newzlib-error)
  ((detail :initarg :detail :initform nil :reader newzlib-error-detail))
  (:report (lambda (condition stream)
             (format stream "cl-newzlib: out of memory~@[: ~A~]"
                     (newzlib-error-detail condition)))))

(define-condition newzlib-end-of-input (newzlib-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "cl-newzlib: unexpected end of compressed input"))))
