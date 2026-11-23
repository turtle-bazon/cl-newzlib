;;; CFFI bindings to the system zlib library, used by CROSS-TESTS to
;;; validate cl-newzlib against the reference implementation.
;;;
;;; Everything here is intentionally small: one-shot deflate/inflate with a
;;; caller-chosen windowBits (raw = -15, zlib = 15, gzip = 31), plus the
;;; Adler-32 and CRC-32 functions for checksum cross-checks.

(in-package #:cl-newzlib-tests)

(defparameter *zlib-loaded* nil)

(defun load-libz ()
  (unless *zlib-loaded*
    (cffi:load-foreign-library "libz.so.1")
    (setf *zlib-loaded* t)))

(defun zlib-available-p ()
  "True when the system zlib shared library could be loaded."
  (handler-case (progn (load-libz) t)
    (error () nil)))

(defconstant +z-no-flush+ 0)
(defconstant +z-partial-flush+ 1)
(defconstant +z-sync-flush+ 2)
(defconstant +z-full-flush+ 3)
(defconstant +z-finish+ 4)
(defconstant +z-deflated+ 8)
(defconstant +z-default-strategy+ 0)
(defconstant +z-huffman-only+ 2)
(defconstant +z-rle+ 3)
(defconstant +z-fixed+ 4)
(defconstant +z-ok+ 0)
(defconstant +z-stream-end+ 1)
(defconstant +z-buf-error+ -5)

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

(cffi:defcfun ("deflateInit2_" %z-deflate-init2) :int
  (strm :pointer) (level :int) (method :int) (window-bits :int)
  (mem-level :int) (strategy :int) (version :string) (stream-size :unsigned-int))

(cffi:defcfun "deflate" :int (strm :pointer) (flush :int))
(cffi:defcfun "deflateEnd" :int (strm :pointer))

(cffi:defcfun ("inflateInit2_" %z-inflate-init2) :int
  (strm :pointer) (window-bits :int) (version :string) (stream-size :unsigned-int))

(cffi:defcfun "inflate" :int (strm :pointer) (flush :int))
(cffi:defcfun "inflateEnd" :int (strm :pointer))

(cffi:defcfun ("compressBound" compress-bound) :unsigned-long (source-len :unsigned-long))

;;; NB: the lisp names must not collide with cl-newzlib's exported
;;; ADLER32/CRC32 (which this package inherits), so use explicit names.
(cffi:defcfun ("adler32" %z-adler32) :unsigned-long
  (adler :unsigned-long) (buf :pointer) (len :unsigned-int))

(cffi:defcfun ("crc32" %z-crc32) :unsigned-long
  (crc :unsigned-long) (buf :pointer) (len :unsigned-int))

(defun %read-bytes (ptr count)
  (let ((v (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count v)
      (setf (aref v i) (cffi:mem-aref ptr :unsigned-char i)))))

(defun grow-foreign-unsigned (old old-cap new-cap)
  (let ((new (cffi:foreign-alloc :unsigned-char :count new-cap)))
    (dotimes (i old-cap)
      (setf (cffi:mem-aref new :unsigned-char i)
            (cffi:mem-aref old :unsigned-char i)))
    (cffi:foreign-free old)
    new))

(defun %z-deflate (octets level window-bits)
  "Compress OCTETS with system zlib.  WINDOW-BITS: -15 raw, 15 zlib, 31 gzip."
  (let* ((n (length octets))
         (src (cffi:foreign-alloc :unsigned-char :count (max n 1)
                                  :initial-contents (if (plusp n) octets #(0))))
         ;; compress-bound covers zlib format only; allow for the gzip
         ;; wrapper (header + CRC + ISIZE) on top.
         (bound (+ (compress-bound n) 64))
         (dst (cffi:foreign-alloc :unsigned-char :count bound))
         (stream (cffi:foreign-alloc '(:struct z-stream))))
    (unwind-protect
         (progn
           ;; zlib only installs its default allocator when ZALLOC/ZFREE are
           ;; NULL, so the struct must be zeroed or garbage pointers get called.
           (cffi:with-foreign-slots ((zalloc zfree opaque) stream (:struct z-stream))
             (setf zalloc (cffi:null-pointer)
                   zfree (cffi:null-pointer)
                   opaque (cffi:null-pointer)))
           (cffi:with-foreign-slots ((next-in avail-in next-out avail-out)
                                     stream (:struct z-stream))
             (setf next-in src avail-in n next-out dst avail-out bound))
           (let ((rc (%z-deflate-init2 stream level +z-deflated+ window-bits 8
                                       +z-default-strategy+
                                       (zlib-version)
                                       (cffi:foreign-type-size '(:struct z-stream)))))
             (unless (zerop rc)
               (error "zlib deflateInit2 failed: ~D" rc)))
           (let ((rc (deflate stream +z-finish+)))
             (unless (= rc 1)
               (error "zlib deflate failed: ~D" rc)))
           (cffi:with-foreign-slots ((total-out) stream (:struct z-stream))
             (deflateEnd stream)
             (%read-bytes dst total-out)))
      (cffi:foreign-free src)
      (cffi:foreign-free dst)
      (cffi:foreign-free stream))))

(defun %z-inflate (octets window-bits)
  "Decompress OCTETS with system zlib.  WINDOW-BITS: -15 raw, 15 zlib, 31
  auto-detect zlib or gzip."
  (let* ((n (length octets))
         (src (cffi:foreign-alloc :unsigned-char :count (max n 1)
                                  :initial-contents (if (plusp n) octets #(0))))
         (cap (max 65536 (* n 8)))
         (dst (cffi:foreign-alloc :unsigned-char :count cap))
         (stream (cffi:foreign-alloc '(:struct z-stream))))
    (unwind-protect
         (progn
           (cffi:with-foreign-slots ((zalloc zfree opaque) stream (:struct z-stream))
             (setf zalloc (cffi:null-pointer)
                   zfree (cffi:null-pointer)
                   opaque (cffi:null-pointer)))
           (cffi:with-foreign-slots ((next-in avail-in next-out avail-out)
                                     stream (:struct z-stream))
             (setf next-in src avail-in n next-out dst avail-out cap))
           (let ((rc (%z-inflate-init2 stream window-bits
                                       (zlib-version)
                                       (cffi:foreign-type-size '(:struct z-stream)))))
             (unless (zerop rc)
               (error "zlib inflateInit2 failed: ~D" rc)))
           (loop
             (let ((rc (inflate stream +z-no-flush+)))
               (when (= rc 1)                ; Z_STREAM_END
                 (return))
               (unless (zerop rc)
                 (error "zlib inflate failed: ~D" rc))
               (cffi:with-foreign-slots ((avail-in avail-out) stream (:struct z-stream))
                 (cond
                   ((zerop avail-out)
                    ;; out of room: grow the output buffer and continue
                    (let* ((new-cap (ash cap 1)))
                      (setf dst (grow-foreign-unsigned dst cap new-cap))
                      (cffi:with-foreign-slots ((next-out) stream (:struct z-stream))
                        (setf next-out (cffi:inc-pointer dst cap)
                              avail-out (- new-cap cap)))
                      (setf cap new-cap)))
                   ((zerop avail-in)
                    (error "zlib inflate: truncated stream"))
                   (t
                    (error "zlib inflate: no progress"))))))
           (cffi:with-foreign-slots ((total-out) stream (:struct z-stream))
             (inflateEnd stream)
             (%read-bytes dst total-out)))
      (cffi:foreign-free src)
      (cffi:foreign-free dst)
      (cffi:foreign-free stream))))

(defun z-raw-deflate (octets &optional (level 6))
  (%z-deflate octets level -15))

(defun z-raw-inflate (octets)
  (%z-inflate octets -15))

(defun z-zlib-deflate (octets &optional (level 6))
  (%z-deflate octets level 15))

(defun z-zlib-inflate (octets)
  (%z-inflate octets 15))

(defun z-gzip-deflate (octets &optional (level 6))
  (%z-deflate octets level 31))

(defun z-gzip-inflate (octets)
  (%z-inflate octets 31))

(defun z-adler32 (octets &optional (start 0) (end (length octets)))
  (let ((buf (cffi:foreign-alloc :unsigned-char :count (max (- end start) 1)
                                 :initial-contents (subseq octets start end))))
    (unwind-protect (%z-adler32 1 buf (- end start))
      (cffi:foreign-free buf))))

(defun z-crc32 (octets &optional (start 0) (end (length octets)))
  (let ((buf (cffi:foreign-alloc :unsigned-char :count (max (- end start) 1)
                                 :initial-contents (subseq octets start end))))
    (unwind-protect (%z-crc32 0 buf (- end start))
      (cffi:foreign-free buf))))

;;; ------------------------------------------------------------------
;;; Persistent streaming streams
;;; ------------------------------------------------------------------
;;;
;;; The one-shot helpers above present a complete stream.  The state tests
;;; need a persistent zlib compressor/inflater so packets can be flushed and
;;; consumed individually, exactly as SSH does.

(defstruct (z-deflate-stream (:constructor %make-z-deflate-stream))
  (ptr nil)
  (ended nil))

(defstruct (z-inflate-stream (:constructor %make-z-inflate-stream))
  (ptr nil)
  (ended nil)
  (finished nil))

(defun %zero-z-stream-allocators (stream)
  (cffi:with-foreign-slots ((zalloc zfree opaque) stream (:struct z-stream))
    (setf zalloc (cffi:null-pointer)
          zfree (cffi:null-pointer)
          opaque (cffi:null-pointer)))
  nil)

(defun make-z-deflate-stream (&key (level 6) (window-bits 15)
                                   (strategy +z-default-strategy+)
                                   (mem-level 8))
  "Create a persistent zlib compressor.  WINDOW-BITS: -15 raw, 15 zlib,
31 gzip.  STRATEGY is one of the +Z-...-STRATEGY+ constants."
  (load-libz)
  (let ((stream (cffi:foreign-alloc '(:struct z-stream))))
    (%zero-z-stream-allocators stream)
    (let ((rc (%z-deflate-init2 stream level +z-deflated+ window-bits
                                mem-level strategy (zlib-version)
                                (cffi:foreign-type-size '(:struct z-stream)))))
      (unless (zerop rc)
        (cffi:foreign-free stream)
        (error "zlib deflateInit2 failed: ~D" rc)))
    (%make-z-deflate-stream :ptr stream)))

(defun z-deflate-stream-flush (state octets &optional (flush +z-sync-flush+))
  "Feed OCTETS to persistent compressor STATE and return all output produced.
FLUSH is a zlib flush constant; the default is Z_SYNC_FLUSH."
  (let* ((n (length octets))
         (src (cffi:foreign-alloc :unsigned-char :count (max n 1)
                                  :initial-contents (if (plusp n) octets #(0))))
         (cap (+ (compress-bound (+ n 2048)) 4096))
         (dst (cffi:foreign-alloc :unsigned-char :count cap)))
    (unwind-protect
         (let ((ptr (z-deflate-stream-ptr state)))
           (cffi:with-foreign-slots ((next-in avail-in next-out avail-out)
                                     ptr (:struct z-stream))
             (setf next-in src avail-in n next-out dst avail-out cap))
           (let ((rc (deflate ptr flush)))
             (unless (member rc (list +z-ok+ +z-stream-end+))
               (error "zlib deflate (flush ~D) failed: ~D" flush rc))
             (cffi:with-foreign-slots ((avail-out) ptr (:struct z-stream))
               (%read-bytes dst (- cap avail-out)))))
      (cffi:foreign-free src)
      (cffi:foreign-free dst))))

(defun z-deflate-stream-end (state)
  (unless (z-deflate-stream-ended state)
    (deflateEnd (z-deflate-stream-ptr state))
    (cffi:foreign-free (z-deflate-stream-ptr state))
    (setf (z-deflate-stream-ptr state) nil
          (z-deflate-stream-ended state) t))
  nil)

(defun make-z-inflate-stream (&key (window-bits 15))
  "Create a persistent zlib decompressor.  WINDOW-BITS: -15 raw, 15 zlib,
31 gzip/auto-detect."
  (load-libz)
  (let ((stream (cffi:foreign-alloc '(:struct z-stream))))
    (%zero-z-stream-allocators stream)
    (let ((rc (%z-inflate-init2 stream window-bits (zlib-version)
                                (cffi:foreign-type-size '(:struct z-stream)))))
      (unless (zerop rc)
        (cffi:foreign-free stream)
        (error "zlib inflateInit2 failed: ~D" rc)))
    (%make-z-inflate-stream :ptr stream)))

(defconstant +z-inflate-chunk+ 65536)

(defun z-inflate-stream-feed (state octets)
  "Feed one flushed packet to persistent decompressor STATE with
Z_SYNC_FLUSH and return all plaintext produced."
  (let* ((n (length octets))
         (src (cffi:foreign-alloc :unsigned-char :count (max n 1)
                                  :initial-contents (if (plusp n) octets #(0))))
         (ptr (z-inflate-stream-ptr state))
         (pieces '()))
    (unwind-protect
         (progn
           (cffi:with-foreign-slots ((next-in avail-in) ptr (:struct z-stream))
             (setf next-in src avail-in n))
           (loop
             (let ((dst (cffi:foreign-alloc :unsigned-char
                                            :count +z-inflate-chunk+)))
               (unwind-protect
                    (let ((rc nil) (produced 0))
                      (cffi:with-foreign-slots ((next-out avail-out) ptr
                                               (:struct z-stream))
                        (setf next-out dst avail-out +z-inflate-chunk+))
                      (setf rc (inflate ptr +z-sync-flush+))
                      (cffi:with-foreign-slots ((avail-out avail-in) ptr
                                               (:struct z-stream))
                        (setf produced (- +z-inflate-chunk+ avail-out))
                        (when (plusp produced)
                          (push (%read-bytes dst produced) pieces))
                        (cond
                          ((= rc +z-stream-end+)
                           (setf (z-inflate-stream-finished state) t)
                           (return))
                          ((= rc +z-buf-error+) (return))
                          ((< rc 0)
                           (error "zlib inflate failed: ~D" rc))
                          ((and (zerop avail-in) (< produced +z-inflate-chunk+))
                           (return))
                          ((and (zerop produced) (plusp avail-in))
                           (error "zlib inflate: no progress")))))
                 (cffi:foreign-free dst))))
           (apply #'concatenate '(vector (unsigned-byte 8))
                  (nreverse pieces)))
      (cffi:foreign-free src))))

(defun z-inflate-stream-end (state)
  (unless (z-inflate-stream-ended state)
    (inflateEnd (z-inflate-stream-ptr state))
    (cffi:foreign-free (z-inflate-stream-ptr state))
    (setf (z-inflate-stream-ptr state) nil
          (z-inflate-stream-ended state) t))
  nil)
