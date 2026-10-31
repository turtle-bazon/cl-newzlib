(defpackage #:cl-newzlib-bench
  (:use #:cl)
  (:export #:main #:run-benchmark))

(in-package #:cl-newzlib-bench)

;;;; Benchmark cl-newzlib against the system C zlib.
;;;;
;;;; The harness measures the same operations on identical input:
;;;;
;;;;   * compress / decompress          -- cl-newzlib's one-shot API against
;;;;                                       a fresh zlib stream per call
;;;;   * compress-into / decompress-into -- cl-newzlib's allocation-free API
;;;;                                       against a reused zlib stream
;;;;                                       (deflateReset / inflateReset)
;;;;
;;;; Input and output buffers live outside the timed region, so the numbers
;;;; primarily reflect the DEFLATE/INFLATE implementations.  Every compressed
;;;; stream is cross-checked: cl-newzlib output is inflated by zlib and zlib
;;;; output is inflated by cl-newzlib before it is timed.
;;;;
;;;; Throughput is reported in MiB/s of *uncompressed* data.  "cl/zlib" is
;;;; cl-newzlib's throughput divided by zlib's; > 1 means cl-newzlib is faster.

;;; ------------------------------------------------------------------
;;; CFFI bindings to the system zlib
;;; ------------------------------------------------------------------

(cffi:defcstruct z-stream
  (next-in :pointer)
  (avail-in :unsigned-int)
  (total-in :unsigned-long)
  (next-out :pointer)
  (avail-out :unsigned-int)
  (total-out :unsigned-long)
  (msg :pointer)
  (state :pointer)
  (zalloc :pointer)
  (zfree :pointer)
  (opaque :pointer)
  (data-type :int)
  (adler :unsigned-long)
  (reserved :unsigned-long))

(cffi:defcfun ("zlibVersion" zlib-version) :string)

(cffi:defcfun ("deflateInit2_" z-deflate-init2) :int
  (strm :pointer) (level :int) (method :int) (window-bits :int)
  (mem-level :int) (strategy :int) (version :string) (stream-size :unsigned-int))

(cffi:defcfun "deflate" :int (strm :pointer) (flush :int))
(cffi:defcfun "deflateReset" :int (strm :pointer))
(cffi:defcfun "deflateEnd" :int (strm :pointer))

(cffi:defcfun ("inflateInit2_" z-inflate-init2) :int
  (strm :pointer) (window-bits :int) (version :string) (stream-size :unsigned-int))

(cffi:defcfun "inflate" :int (strm :pointer) (flush :int))
(cffi:defcfun "inflateReset" :int (strm :pointer))
(cffi:defcfun "inflateEnd" :int (strm :pointer))

(cffi:defcfun ("compressBound" z-compress-bound) :unsigned-long
  (source-len :unsigned-long))

(defvar *zlib-loaded* nil)

(defun ensure-zlib ()
  (unless *zlib-loaded*
    (handler-case
        (progn
          (cffi:load-foreign-library "libz.so.1")
          (setf *zlib-loaded* t))
      (error (e)
        (error "could not load the system zlib (libz.so.1): ~A" e)))))

(defconstant +z-ok+ 0)
(defconstant +z-stream-end+ 1)
(defconstant +z-buf-error+ -5)
(defconstant +z-no-flush+ 0)
(defconstant +z-finish+ 4)
(defconstant +z-deflated+ 8)
(defconstant +z-default-strategy+ 0)

(defun zwindow-bits (format)
  (ecase format
    (:zlib 15)
    (:gzip 31)
    (:raw -15)))

(defun zformat-name (format)
  (ecase format (:zlib "zlib") (:gzip "gzip") (:raw "raw")))

(defun zcheck (rc what)
  (unless (= rc +z-ok+)
    (error "~A failed with zlib error ~D" what rc)))

(defun zslot (stream slot)
  (cffi:foreign-slot-value stream '(:struct z-stream) slot))

(defun (setf zslot) (value stream slot)
  (setf (cffi:foreign-slot-value stream '(:struct z-stream) slot) value))

(defun set-stream-io (stream in n out out-cap)
  (setf (zslot stream 'next-in) in
        (zslot stream 'avail-in) n
        (zslot stream 'next-out) out
        (zslot stream 'avail-out) out-cap))

(defun new-zstream ()
  "Allocate a zeroed z_stream.  A zeroed zalloc/zfree lets zlib install its
default allocator; leaving them uninitialized would call garbage pointers."
  (let ((stream (cffi:foreign-alloc '(:struct z-stream))))
    (dotimes (i (cffi:foreign-type-size '(:struct z-stream)))
      (setf (cffi:mem-aref stream :unsigned-char i) 0))
    stream))

(defun octets-to-foreign (octets)
  (cffi:foreign-array-alloc octets
                            (list :array :unsigned-char (length octets))))

(defun foreign-to-octets (pointer count)
  "Copy COUNT octets from POINTER into a fresh Lisp vector.  Use memcpy rather
than CFFI's element-by-element FOREIGN-ARRAY-TO-LISP, which would dominate the
one-shot timings."
  (let ((out (make-array count :element-type '(unsigned-byte 8))))
    (when (plusp count)
      (cffi:with-pointer-to-vector-data (destination out)
        (cffi:foreign-funcall "memcpy"
                              :pointer destination
                              :pointer pointer
                              :size count
                              :pointer)))
    out))

(defun deflate-loop (stream)
  (loop
    (let ((rc (deflate stream +z-finish+)))
      (cond
        ((= rc +z-stream-end+) (return))
        ((= rc +z-ok+)
         (when (zerop (zslot stream 'avail-out))
           (error "zlib deflate: output buffer too small")))
        (t (error "zlib deflate failed with error ~D" rc))))))

(defun inflate-loop (stream)
  (loop
    (let ((rc (inflate stream +z-no-flush+)))
      (cond
        ((= rc +z-stream-end+) (return))
        ((= rc +z-ok+)
         (when (zerop (zslot stream 'avail-out))
           (error "zlib inflate: output buffer too small")))
        ((= rc +z-buf-error+)
         (error "zlib inflate: no progress (truncated stream?)"))
        (t (error "zlib inflate failed with error ~D" rc))))))

;;; Persistent (reset/reused) compressor and decompressor.  These model the
;;; "into" API and avoid charging zlib for per-call state allocation.

(defstruct (zcompressor (:constructor %make-zcompressor))
  (stream nil)
  (in nil)
  (n 0)
  (out nil)
  (out-cap 0))

(defun make-zcompressor (octets level window-bits out-cap)
  (let* ((n (length octets))
         (in (octets-to-foreign octets))
         (out (cffi:foreign-alloc :unsigned-char :count out-cap))
         (stream (new-zstream)))
    (handler-case
        (progn
          (zcheck (z-deflate-init2 stream level +z-deflated+ window-bits 8
                                   +z-default-strategy+ (zlib-version)
                                   (cffi:foreign-type-size '(:struct z-stream)))
                  "deflateInit2")
          (%make-zcompressor :stream stream :in in :n n :out out
                             :out-cap out-cap))
      (error (e)
        (cffi:foreign-free in)
        (cffi:foreign-free out)
        (cffi:foreign-free stream)
        (error e)))))

(defun free-zcompressor (zc)
  (when zc
    (deflateEnd (zcompressor-stream zc))
    (cffi:foreign-free (zcompressor-in zc))
    (cffi:foreign-free (zcompressor-out zc))
    (cffi:foreign-free (zcompressor-stream zc))))

(defun zcompressor-run (zc)
  "Compress into the reused stream; return the compressed size."
  (let ((stream (zcompressor-stream zc)))
    (zcheck (deflateReset stream) "deflateReset")
    (set-stream-io stream (zcompressor-in zc) (zcompressor-n zc)
                   (zcompressor-out zc) (zcompressor-out-cap zc))
    (deflate-loop stream)
    (zslot stream 'total-out)))

(defstruct (zinflater (:constructor %make-zinflater))
  (stream nil)
  (in nil)
  (n 0)
  (out nil)
  (out-cap 0))

(defun make-zinflater (octets window-bits out-cap)
  (let* ((n (length octets))
         (in (octets-to-foreign octets))
         (out (cffi:foreign-alloc :unsigned-char :count out-cap))
         (stream (new-zstream)))
    (handler-case
        (progn
          (zcheck (z-inflate-init2 stream window-bits (zlib-version)
                                   (cffi:foreign-type-size '(:struct z-stream)))
                  "inflateInit2")
          (%make-zinflater :stream stream :in in :n n :out out :out-cap out-cap))
      (error (e)
        (cffi:foreign-free in)
        (cffi:foreign-free out)
        (cffi:foreign-free stream)
        (error e)))))

(defun free-zinflater (zi)
  (when zi
    (inflateEnd (zinflater-stream zi))
    (cffi:foreign-free (zinflater-in zi))
    (cffi:foreign-free (zinflater-out zi))
    (cffi:foreign-free (zinflater-stream zi))))

(defun zinflater-run (zi)
  "Inflate into the reused stream; return the uncompressed size."
  (let ((stream (zinflater-stream zi)))
    (zcheck (inflateReset stream) "inflateReset")
    (set-stream-io stream (zinflater-in zi) (zinflater-n zi)
                   (zinflater-out zi) (zinflater-out-cap zi))
    (inflate-loop stream)
    (zslot stream 'total-out)))

;;; Fresh-state variants.  These mirror the one-shot API: zlib pays for
;;; deflateInit2/inflateInit2 and teardown on every call, and the result is
;;; copied into a fresh Lisp octet vector just like compress-octets et al.

(defun zcompress-fresh (zc level window-bits)
  (let ((stream (new-zstream))
        (initialized nil))
    (unwind-protect
         (progn
           (zcheck (z-deflate-init2 stream level +z-deflated+ window-bits 8
                                    +z-default-strategy+ (zlib-version)
                                    (cffi:foreign-type-size '(:struct z-stream)))
                   "deflateInit2")
           (setf initialized t)
           (set-stream-io stream (zcompressor-in zc) (zcompressor-n zc)
                          (zcompressor-out zc) (zcompressor-out-cap zc))
           (deflate-loop stream)
           (foreign-to-octets (zcompressor-out zc) (zslot stream 'total-out)))
      (when initialized (deflateEnd stream))
      (cffi:foreign-free stream))))

(defun zinflate-fresh (zi window-bits)
  (let ((stream (new-zstream))
        (initialized nil))
    (unwind-protect
         (progn
           (zcheck (z-inflate-init2 stream window-bits (zlib-version)
                                    (cffi:foreign-type-size '(:struct z-stream)))
                   "inflateInit2")
           (setf initialized t)
           (set-stream-io stream (zinflater-in zi) (zinflater-n zi)
                          (zinflater-out zi) (zinflater-out-cap zi))
           (inflate-loop stream)
           (foreign-to-octets (zinflater-out zi) (zslot stream 'total-out)))
      (when initialized (inflateEnd stream))
      (cffi:foreign-free stream))))

(defun zinflate-octets (octets window-bits uncompressed-size)
  "Convenience: inflate OCTETS with zlib and return a Lisp vector."
  (let ((zi (make-zinflater octets window-bits (+ uncompressed-size 64))))
    (unwind-protect
         (zinflate-fresh zi window-bits)
      (free-zinflater zi))))

;;; ------------------------------------------------------------------
;;; Datasets
;;; ------------------------------------------------------------------

(defparameter *default-size* (* 2 1024 1024))

(defun project-root ()
  (asdf:system-source-directory "cl-newzlib"))

(defun read-file-octets (path &key max)
  (with-open-file (stream path :direction :input
                               :element-type '(unsigned-byte 8))
    (let* ((available (file-length stream))
           (n (if max (min available max) available))
           (octets (make-array n :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun tile-octets (octets n)
  "Return N octets taken from OCTETS, repeating it when it is shorter."
  (let ((m (length octets)))
    (cond
      ((zerop m) (error "cannot tile an empty octet vector"))
      ((= n m) octets)
      ((< n m) (subseq octets 0 n))
      (t
       (let ((out (make-array n :element-type '(unsigned-byte 8))))
         (loop for start from 0 below n by m
               do (replace out octets
                           :start1 start
                           :end1 (min n (+ start m))))
         out)))))

(defun concatenate-octets (parts)
  (let* ((total (reduce #'+ parts :key #'length :initial-value 0))
         (out (make-array total :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (part parts out)
      (replace out part :start1 pos)
      (incf pos (length part)))))

(defun repo-source-octets ()
  (let ((files (sort (directory (merge-pathnames "src/*.lisp" (project-root)))
                     #'string< :key #'pathname-name)))
    (concatenate-octets (mapcar #'read-file-octets files))))

(defun make-random-octets (n &optional (seed #x9E3779B9))
  (let ((out (make-array n :element-type '(unsigned-byte 8)))
        (x (logand seed #xFFFFFFFF)))
    (dotimes (i n out)
      (setf x (logand #xFFFFFFFF (logxor x (ash x 13))))
      (setf x (logand #xFFFFFFFF (logxor x (ash x -17))))
      (setf x (logand #xFFFFFFFF (logxor x (ash x 5))))
      (setf (aref out i) (logand x #xFF)))))

(defun first-existing (paths)
  (find-if #'probe-file paths))

(defun make-dataset (name size)
  (ecase name
    (:text
     (let ((path (first-existing
                  (list "/usr/share/doc/rclone/MANUAL.html"
                        "/usr/share/doc/git/changelog.gz"
                        "/usr/share/doc/fish/relnotes.html"))))
       (if path
           (list :name "text"
                 :description (namestring path)
                 :data (tile-octets (read-file-octets path :max size) size))
           (list :name "text"
                 :description "repository source"
                 :data (tile-octets (repo-source-octets) size)))))
    (:source
     (list :name "source"
           :description "repository src/*.lisp"
           :data (tile-octets (repo-source-octets) size)))
    (:binary
     (let ((path (first-existing
                  (list "/usr/local/bin/sbcl"
                        "/usr/bin/sbcl"
                        "/usr/bin/bash"
                        "/bin/bash"))))
       (unless path
         (error "no binary dataset file found"))
       (list :name "binary"
             :description (namestring path)
             :data (tile-octets (read-file-octets path :max size) size))))
    (:zeros
     (list :name "zeros"
           :description "generated zeroes"
           :data (make-array size :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
    (:random
     (list :name "random"
           :description "generated xorshift32 bytes"
           :data (make-random-octets size)))))

(defparameter *dataset-names* '(:text :source :binary :zeros :random))
(defparameter *default-datasets* '(:text :binary :zeros :random))

;;; ------------------------------------------------------------------
;;; Timing
;;; ------------------------------------------------------------------

(defun full-gc ()
  #+sbcl (sb-ext:gc :full t)
  #-sbcl (values))

(defun median (values)
  (let* ((sorted (sort (copy-list values) #'<))
         (n (length sorted)))
    (nth (floor n 2) sorted)))

(defun time-reps (thunk reps)
  (let ((start (get-internal-real-time)))
    (dotimes (i reps)
      (declare (ignore i))
      (funcall thunk))
    (/ (- (get-internal-real-time) start)
       (* 1.0 internal-time-units-per-second reps))))

(defun measure (thunk &key (target 0.2) (samples 3) (max-reps 1000))
  "Return (values MEDIAN BEST REPS) seconds per call."
  (funcall thunk)                       ; warm up and size the repetition count
  (let* ((one (max (time-reps thunk 1) 1e-7))
         (reps (max 1 (min max-reps (floor target one))))
         (times (loop repeat samples
                      collect (progn
                                (full-gc)
                                (time-reps thunk reps)))))
    (values (median times) (reduce #'min times) reps)))

(defun mib-per-second (octets seconds)
  (/ octets 1048576.0 seconds))

(defun speed-ratio (a b)
  (if (zerop b) 0.0 (/ a b)))

(defun geometric-mean (values)
  (let ((values (remove-if-not #'plusp values)))
    (if values
        (exp (/ (loop for value in values sum (log value)) (length values)))
        0.0)))

;;; ------------------------------------------------------------------
;;; Benchmark core
;;; ------------------------------------------------------------------

(defstruct bench-result
  (level 0)
  (mode :standard)
  (cl-size 0)
  (z-size 0)
  (cl-comp 0.0)
  (z-comp 0.0)
  (cl-decomp 0.0)
  (z-decomp 0.0)
  (cl-comp-1 0.0)
  (z-comp-1 0.0)
  (cl-decomp-1 0.0)
  (z-decomp-1 0.0))

(defstruct dataset-result
  (name "")
  (description "")
  (data #())
  (results '()))

(defun compressed-capacity (n format)
  "A comfortable worst case for both cl-newzlib's compress-into and zlib."
  (+ (max (+ n (ash n -3)) (z-compress-bound n))
     (ecase format (:zlib 64) (:gzip 128) (:raw 512))))

(defun cl-decompress (octets format)
  (cl-newzlib:decompress-octets octets :format format))

(defun validate-streams (data cl-compressed z-compressed format window-bits)
  (unless (equalp data (cl-decompress cl-compressed format))
    (error "cl-newzlib output did not inflate with cl-newzlib"))
  (unless (equalp data (cl-decompress z-compressed format))
    (error "zlib output did not inflate with cl-newzlib"))
  (unless (equalp data (zinflate-octets cl-compressed window-bits (length data)))
    (error "cl-newzlib output did not inflate with zlib"))
  (unless (equalp data (zinflate-octets z-compressed window-bits (length data)))
    (error "zlib output did not inflate with zlib")))

(defun benchmark-level (data level mode format samples target)
  (let* ((n (length data))
         (window-bits (zwindow-bits format))
         (capacity (compressed-capacity n format))
         (cl-out (make-array capacity :element-type '(unsigned-byte 8)))
         (cl-size (cl-newzlib:compress-into cl-out data :format format
                                            :level level :mode mode))
         (cl-compressed (subseq cl-out 0 cl-size))
         (zc (make-zcompressor data level window-bits capacity)))
    (unwind-protect
         (let* ((z-size (zcompressor-run zc))
                (z-compressed (foreign-to-octets (zcompressor-out zc) z-size)))
           (validate-streams data cl-compressed z-compressed format window-bits)
           (let ((zi (make-zinflater z-compressed window-bits (+ n 64)))
                 (d-out (make-array n :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (let ((cl-comp-core (measure
                                       (lambda ()
                                         (cl-newzlib:compress-into
                                          cl-out data :format format
                                          :level level :mode mode))
                                       :target target :samples samples))
                        (z-comp-core (measure
                                      (lambda () (zcompressor-run zc))
                                      :target target :samples samples))
                        (cl-decomp-core (measure
                                         (lambda ()
                                           (cl-newzlib:decompress-into
                                            d-out z-compressed :format format))
                                         :target target :samples samples))
                        (z-decomp-core (measure
                                        (lambda () (zinflater-run zi))
                                        :target target :samples samples))
                        (cl-comp-1 (measure
                                    (lambda ()
                                      (cl-newzlib:compress-octets
                                       data :format format
                                       :level level :mode mode))
                                    :target target :samples samples))
                        (z-comp-1 (measure
                                   (lambda () (zcompress-fresh zc level window-bits))
                                   :target target :samples samples))
                        (cl-decomp-1 (measure
                                      (lambda ()
                                        (cl-newzlib:decompress-octets
                                         z-compressed :format format))
                                      :target target :samples samples))
                        (z-decomp-1 (measure
                                     (lambda ()
                                       (zinflate-fresh zi window-bits))
                                     :target target :samples samples)))
                    (make-bench-result
                     :level level
                     :mode mode
                     :cl-size cl-size
                     :z-size z-size
                     :cl-comp cl-comp-core
                     :z-comp z-comp-core
                     :cl-decomp cl-decomp-core
                     :z-decomp z-decomp-core
                     :cl-comp-1 cl-comp-1
                     :z-comp-1 z-comp-1
                     :cl-decomp-1 cl-decomp-1
                     :z-decomp-1 z-decomp-1))
               (free-zinflater zi))))
      (free-zcompressor zc))))

;;; ------------------------------------------------------------------
;;; Reporting
;;; ------------------------------------------------------------------

(defun print-header (options)
  (format t "~&=== cl-newzlib vs zlib ===~%")
  (format t "Lisp:   ~A ~A~%" (lisp-implementation-type)
          (lisp-implementation-version))
  (format t "zlib:   ~A~%" (zlib-version))
  (format t "Format: ~A   Size: ~,2f MiB   Mode: ~A   Samples: ~D   Target: ~,2fs~%"
          (zformat-name (getf options :format))
          (/ (getf options :size) 1048576.0)
          (string-downcase (symbol-name (getf options :mode)))
          (getf options :samples)
          (getf options :target))
  (format t "Levels: ~{~D~^, ~}~%" (getf options :levels))
  (let ((cpu (ignore-errors
               (with-open-file (stream "/proc/cpuinfo")
                 (loop for line = (read-line stream nil)
                       while line
                       when (search "model name" line)
                         do (return (string-trim " "
                                                 (subseq line
                                                         (1+ (position #\: line))))))))))
    (when cpu (format t "CPU:    ~A~%" cpu)))
  (format t "~%"))

(defun print-table-row (fmt &rest args)
  (apply #'format t fmt args)
  (terpri))

(defun print-compression-table (results data-size)
  (print-table-row "| level | cl size | zlib size | cl % | zlib % | cl MiB/s | zlib MiB/s | cl/zlib |")
  (print-table-row "|------:|--------:|----------:|-----:|-------:|---------:|-----------:|--------:|")
  (dolist (r results)
    (let ((level (bench-result-level r))
          (cl-size (bench-result-cl-size r))
          (z-size (bench-result-z-size r))
          (cl (/ data-size 1048576.0 (bench-result-cl-comp r)))
          (z (/ data-size 1048576.0 (bench-result-z-comp r))))
      (print-table-row "| ~D | ~D | ~D | ~,2f% | ~,2f% | ~,1f | ~,1f | ~,2fx |"
                       level cl-size z-size
                       (* 100.0 (/ cl-size data-size))
                       (* 100.0 (/ z-size data-size))
                       cl z (speed-ratio cl z)))))

(defun print-decompression-table (results data-size)
  (print-table-row "| level | cl MiB/s | zlib MiB/s | cl/zlib |")
  (print-table-row "|------:|---------:|-----------:|--------:|")
  (dolist (r results)
    (let ((cl (/ data-size 1048576.0 (bench-result-cl-decomp r)))
          (z (/ data-size 1048576.0 (bench-result-z-decomp r))))
      (print-table-row "| ~D | ~,1f | ~,1f | ~,2fx |"
                       (bench-result-level r) cl z (speed-ratio cl z)))))

(defun print-one-shot-table (results data-size)
  (print-table-row "| level | cl comp | zlib comp | cl/zlib | cl decomp | zlib decomp | cl/zlib |")
  (print-table-row "|------:|--------:|----------:|--------:|----------:|------------:|--------:|")
  (dolist (r results)
    (let ((clc (/ data-size 1048576.0 (bench-result-cl-comp-1 r)))
          (zc (/ data-size 1048576.0 (bench-result-z-comp-1 r)))
          (cld (/ data-size 1048576.0 (bench-result-cl-decomp-1 r)))
          (zd (/ data-size 1048576.0 (bench-result-z-decomp-1 r))))
      (print-table-row "| ~D | ~,1f | ~,1f | ~,2fx | ~,1f | ~,1f | ~,2fx |"
                       (bench-result-level r) clc zc (speed-ratio clc zc)
                       cld zd (speed-ratio cld zd)))))

(defun print-dataset-report (dataset)
  (let* ((data (dataset-result-data dataset))
         (n (length data))
         (results (dataset-result-results dataset)))
    (format t "## ~A — ~A (~,2f MiB)~%~%"
            (dataset-result-name dataset) (dataset-result-description dataset)
            (/ n 1048576.0))
    (format t "### compression (into, reused buffers)~%~%")
    (print-compression-table results n)
    (format t "~%### decompression (into, identical zlib stream)~%~%")
    (print-decompression-table results n)
    (format t "~%### one-shot API~%~%")
    (print-one-shot-table results n)
    (format t "~%")))

(defun ratios-for-results (results accessor-a accessor-b)
  (loop for r in results
        collect (speed-ratio (funcall accessor-b r)
                             (funcall accessor-a r))))

(defun print-summary-row (label results)
  (print-table-row "| ~A | ~,2fx | ~,2fx | ~,2fx | ~,2fx |"
                   label
                   (geometric-mean
                    (ratios-for-results results
                                        #'bench-result-cl-comp
                                        #'bench-result-z-comp))
                   (geometric-mean
                    (ratios-for-results results
                                        #'bench-result-cl-decomp
                                        #'bench-result-z-decomp))
                   (geometric-mean
                    (ratios-for-results results
                                        #'bench-result-cl-comp-1
                                        #'bench-result-z-comp-1))
                   (geometric-mean
                    (ratios-for-results results
                                        #'bench-result-cl-decomp-1
                                        #'bench-result-z-decomp-1))))

(defun print-summary (all-results)
  (format t "## Summary (geometric mean cl/zlib, >1 means cl-newzlib faster)~%~%")
  (print-table-row "| dataset | compress (into) | decompress (into) | compress (1-shot) | decompress (1-shot) |")
  (print-table-row "|---------|----------------:|------------------:|------------------:|--------------------:|")
  (dolist (dataset all-results)
    (print-summary-row (dataset-result-name dataset)
                       (dataset-result-results dataset)))
  (let ((all (loop for dataset in all-results
                   append (dataset-result-results dataset))))
    (print-summary-row "**all**" all))
  (format t "~%"))

;;; ------------------------------------------------------------------
;;; Command line
;;; ------------------------------------------------------------------

(defun usage ()
  (format t "~&Usage: bench/run.sh [options]~%~%")
  (format t "  --size N          uncompressed dataset size (default 2MiB; K/M/G suffixes)~%")
  (format t "  --levels L1,L2    compression levels (default 1,3,6,9)~%")
  (format t "  --datasets LIST   text,source,binary,zeros,random (default text,binary,zeros,random)~%")
  (format t "  --mode MODE       cl-newzlib mode: standard or fast (default standard)~%")
  (format t "  --format FORMAT   zlib, gzip or raw (default zlib)~%")
  (format t "  --samples N       timed samples per measurement (default 3)~%")
  (format t "  --target SECONDS  minimum time per sample (default 0.2)~%")
  (format t "  --quick           --size 1MiB --levels 1,6 --samples 1 --target 0.1~%")
  (format t "  --help            show this help~%"))

(defun parse-size (string)
  (multiple-value-bind (number end)
      (parse-integer string :junk-allowed t)
    (unless number
      (error "invalid size: ~S" string))
    (let* ((suffix (string-downcase (string-trim " " (subseq string end))))
           (multiplier (cond
                         ((member suffix '("" "b") :test #'string=) 1)
                         ((member suffix '("k" "kb" "kib") :test #'string=) 1024)
                         ((member suffix '("m" "mb" "mib") :test #'string=)
                          (* 1024 1024))
                         ((member suffix '("g" "gb" "gib") :test #'string=)
                          (* 1024 1024 1024))
                         (t (error "invalid size suffix: ~S" suffix)))))
      (* number multiplier))))

(defun split-comma (string)
  (loop with start = 0
        for comma = (position #\, string :start start)
        collect (string-trim " " (subseq string start comma))
        while comma
        do (setf start (1+ comma))))

(defun parse-levels (string)
  (mapcar (lambda (part)
            (or (parse-integer part :junk-allowed nil)
                (error "invalid level: ~S" part)))
          (split-comma string)))

(defun parse-datasets (string)
  (mapcar (lambda (part)
            (let ((keyword (intern (string-upcase part) :keyword)))
              (unless (member keyword *dataset-names*)
                (error "unknown dataset: ~S (expected one of ~{~A~^, ~})"
                       part (mapcar #'symbol-name *dataset-names*)))
              keyword))
          (split-comma string)))

(defun parse-args (argv)
  (let ((options (list :size *default-size*
                       :levels '(1 3 6 9)
                       :datasets *default-datasets*
                       :mode :standard
                       :format :zlib
                       :samples 3
                       :target 0.2)))
    (loop while argv
          for key = (pop argv)
          do (cond
               ((string= key "--help")
                (usage)
                (sb-ext:exit :code 0))
               ((string= key "--quick")
                (setf (getf options :size) (* 1024 1024)
                      (getf options :levels) '(1 6)
                      (getf options :samples) 1
                      (getf options :target) 0.1))
               ((string= key "--size")
                (setf (getf options :size) (parse-size (pop argv))))
               ((string= key "--levels")
                (setf (getf options :levels) (parse-levels (pop argv))))
               ((string= key "--datasets")
                (setf (getf options :datasets) (parse-datasets (pop argv))))
               ((string= key "--mode")
                (let ((mode (intern (string-upcase (pop argv)) :keyword)))
                  (unless (member mode '(:standard :fast))
                    (error "invalid mode: ~S" mode))
                  (setf (getf options :mode) mode)))
               ((string= key "--format")
                (let ((format (intern (string-upcase (pop argv)) :keyword)))
                  (unless (member format '(:zlib :gzip :raw))
                    (error "invalid format: ~S" format))
                  (setf (getf options :format) format)))
               ((string= key "--samples")
                (setf (getf options :samples) (parse-integer (pop argv))))
               ((string= key "--target")
                (setf (getf options :target) (read-from-string (pop argv))))
               (t (error "unknown option: ~S (try --help)" key))))
    options))

(defun run-benchmark (&optional (options nil))
  (setf options (or options (parse-args nil)))
  (ensure-zlib)
  (print-header options)
  (let ((all-results '()))
    (dolist (name (getf options :datasets))
      (let* ((spec (make-dataset name (getf options :size)))
             (data (getf spec :data))
             (results
               (loop for level in (getf options :levels)
                     collect (benchmark-level data level (getf options :mode)
                                              (getf options :format)
                                              (getf options :samples)
                                              (getf options :target))))
             (dataset (make-dataset-result
                       :name (getf spec :name)
                       :description (getf spec :description)
                       :data data
                       :results results)))
        (print-dataset-report dataset)
        (push dataset all-results)))
    (print-summary (nreverse all-results))))

(defun main ()
  (let ((argv (rest sb-ext:*posix-argv*)))
    (when (and argv (string= (first argv) "--"))
      (pop argv))
    (handler-case
        (run-benchmark (parse-args argv))
      (error (e)
        (format *error-output* "~&benchmark failed: ~A~%" e)
        (sb-ext:exit :code 1)))))
