(in-package #:cl-newzlib)

;;; Public one-shot API.
;;;
;;; Low-level entry points are the format wrappers:
;;;   RAW-DEFLATE / RAW-INFLATE    (RFC 1951, no wrapper)
;;;   ZLIB-COMPRESS / ZLIB-DECOMPRESS  (RFC 1950)
;;;   GZIP-COMPRESS / GZIP-DECOMPRESS  (RFC 1952)
;;;
;;; COMPRESS-OCTETS / DECOMPRESS-OCTETS dispatch on a FORMAT keyword, and
;;; COMPRESS / DECOMPRESS additionally accept a pathname or binary stream.

;;; ------------------------------------------------------------------
;;; Format dispatch
;;; ------------------------------------------------------------------

(defun check-format (format)
  (unless (member format '(:zlib :gzip :raw))
    (error 'newzlib-parameter-error
           :detail (format nil "unsupported format ~S (expected :zlib, :gzip or :raw)"
                           format)))
  format)

(defun raw-deflate (input &optional (start 0) (end (length input))
                          (level +default-compression+))
  "Raw DEFLATE compression (RFC 1951), no wrapper."
  (deflate-raw input start end level))

(defun raw-inflate (input &optional (start 0) (end (length input)))
  "Raw DEFLATE decompression (RFC 1951), no wrapper."
  (inflate-raw input start end))

(defun compress-octets (octets &key (format :zlib)
                               (level +default-compression+))
  "Compress OCTETS.  FORMAT is :zlib (default), :gzip or :raw.  Returns a
  fresh octet vector."
  (declare (type simple-array octets))
  (check-format format)
  (case format
    (:zlib (zlib-compress octets 0 (length octets) level))
    (:gzip (gzip-compress octets 0 (length octets) level))
    (:raw (deflate-raw octets 0 (length octets) level))))

(defun compress-into (output octets &key (format :zlib)
                                      (level +default-compression+))
  "Compress OCTETS into the caller-owned simple octet vector OUTPUT and return
the number of octets written.  The first returned-count elements of OUTPUT
are valid and its remaining elements are unchanged.  OUTPUT must have the
format-specific worst-case capacity; level 0 requires only its exact stored
output size.  OUTPUT must not alias OCTETS."
  (declare (type simple-array output octets))
  (check-format format)
  (case format
    (:zlib (zlib-compress-into output octets level))
    (:gzip (gzip-compress-into output octets level))
    (:raw (deflate-raw-into output octets level))))

(defun decompress-into (output octets &key (format :zlib))
  "Decompress OCTETS into the caller-owned simple octet vector OUTPUT and
return the number of octets written.  OUTPUT must have sufficient capacity,
must not alias OCTETS, and its elements after the returned count are left
unchanged."
  (declare (type simple-array output octets))
  (check-format format)
  (case format
    (:zlib (zlib-decompress-into output octets))
    (:gzip (gzip-decompress-into output octets))
    (:raw (inflate-raw-into output octets))))

(defun decompress-octets (octets &key (format :zlib))
  "Decompress OCTETS, a stream in the given FORMAT (:zlib, :gzip or :raw).
  Returns a fresh octet vector."
  (declare (type simple-array octets))
  (check-format format)
  (case format
    (:zlib (zlib-decompress octets))
    (:gzip (gzip-decompress octets))
    (:raw (inflate-raw octets))))

;;; ------------------------------------------------------------------
;;; Stream / pathname convenience
;;; ------------------------------------------------------------------

(defun read-all-octets (source)
  "Read all octets from SOURCE (a binary input stream or a pathname
  designator) into a fresh octet vector."
  (if (streamp source)
      (let ((size 4096)
            (pos 0))
        (declare (type fixnum size pos))
        (let ((out (make-octet-buffer size))
              (buffer (make-octet-buffer 4096)))
          (loop for n = (read-sequence buffer source)
                while (plusp n) do
                  (when (> (+ pos n) size)
                    (let ((new-size (max (* 2 size) (+ pos n))))
                      (declare (type fixnum new-size))
                      (let ((bigger (make-octet-buffer new-size)))
                        (replace bigger out :end2 pos)
                        (setf out bigger
                              size new-size))))
                  (replace out buffer :start1 pos :end1 (+ pos n) :end2 n)
                  (incf pos n))
          (let ((result (make-octet-buffer pos)))
            (replace result out :end2 pos)
            result)))
      (with-open-file (s (pathname source) :direction :input
                         :element-type '(unsigned-byte 8))
        (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
          (read-sequence v s)
          v))))

(defun compress (source &key (format :zlib) (level +default-compression+))
  "Compress the contents of SOURCE (a pathname or binary input stream).
  Returns a fresh octet vector in the given FORMAT."
  (compress-octets (read-all-octets source) :format format :level level))

(defun decompress (source &key (format :zlib))
  "Decompress the contents of SOURCE (a pathname or binary input stream).
  Returns a fresh octet vector."
  (decompress-octets (read-all-octets source) :format format))
