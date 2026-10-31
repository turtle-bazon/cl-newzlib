(in-package #:cl-newzlib)

;;; CPU-dependent implementation (re)selection after image startup.
;;;
;;; SB-CPU-FEATURES caches detection and registers its own re-probe hook, but
;;; SB-EXT:*INIT-HOOKS* is a stack: hooks pushed later run first.  Refresh
;;; explicitly before re-selecting, so an image moved to another machine
;;; selects implementations for the machine it is actually running on.

#+sbcl
(defun refresh-cpu-dependent-implementations ()
  "Re-probe the CPU, then re-select every CPU-dependent implementation."
  (sb-cpu-features:refresh-cpu-features)
  #+newzlib-avx2
  (update-simd-adler-impl)
  #+(and sbcl x86-64 cl-newzlib-le newzlib-pclmul)
  (update-pclmul-crc-impl))

#+sbcl
(pushnew 'refresh-cpu-dependent-implementations sb-ext:*init-hooks*)
