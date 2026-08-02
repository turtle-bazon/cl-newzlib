(in-package #:cl-newzlib-tests)

(in-suite cl-newzlib-suite)

;;; Cross-validation against the system zlib via CFFI.  These tests are
;;; skipped automatically when libz.so.1 cannot be loaded.

(test cross-checksum-adler32
  (if (zlib-available-p)
      (loop for n in '(0 1 100 1000 10000)
            do (let ((data (if (zerop n) (empty-data) (random-octets n))))
                 (is (= (cl-newzlib:adler32 data) (z-adler32 data))
                     (format nil "adler32 n=~D" n))))
      (skip "system zlib not available")))

(test cross-checksum-crc32
  (if (zlib-available-p)
      (loop for n in '(0 1 100 1000 10000)
            do (let ((data (if (zerop n) (empty-data) (random-octets n))))
                 (is (= (cl-newzlib:crc32 data) (z-crc32 data))
                     (format nil "crc32 n=~D" n))))
      (skip "system zlib not available")))

(test cross-cl-deflate-z-inflate-raw
  "Our raw DEFLATE output must decode under system zlib."
  (if (zlib-available-p)
      (loop for (name . maker) in +sample-datasets+
            do (let ((data (funcall maker 20000)))
                 (loop for level in '(0 1 6 9)
                       do (let* ((c (compress-octets data :format :raw :level level))
                                 (d (z-raw-inflate c)))
                            (is (equalp data d)
                                (format nil "cl->z raw ~A level ~D" name level))))))
      (skip "system zlib not available")))

(test cross-z-deflate-cl-inflate-raw
  "System zlib's raw DEFLATE output must decode under our inflate."
  (if (zlib-available-p)
      (loop for (name . maker) in +sample-datasets+
            do (let ((data (funcall maker 20000)))
                 (loop for level in '(0 1 6 9)
                       do (let* ((c (z-raw-deflate data level))
                                 (d (decompress-octets c :format :raw)))
                            (is (equalp data d)
                                (format nil "z->cl raw ~A level ~D" name level))))))
      (skip "system zlib not available")))

(test cross-cl-zlib-z-inflate
  "Our zlib-wrapped output must decompress under system zlib uncompress."
  (if (zlib-available-p)
      (loop for (name . maker) in +sample-datasets+
            do (let ((data (funcall maker 20000)))
                 (loop for level in '(0 1 6 9)
                       do (let* ((c (compress-octets data :format :zlib :level level))
                                 (d (z-zlib-inflate c)))
                            (is (equalp data d)
                                (format nil "cl->z zlib ~A level ~D" name level))))))
      (skip "system zlib not available")))

(test cross-z-zlib-cl-inflate
  "System zlib's zlib-wrapped output must decompress under our zlib-decompress."
  (if (zlib-available-p)
      (loop for (name . maker) in +sample-datasets+
            do (let ((data (funcall maker 20000)))
                 (loop for level in '(0 1 6 9)
                       do (let* ((c (z-zlib-deflate data level))
                                 (d (decompress-octets c :format :zlib)))
                            (is (equalp data d)
                                (format nil "z->cl zlib ~A level ~D" name level))))))
      (skip "system zlib not available")))

(test cross-cl-gzip-z-inflate
  "Our gzip-wrapped output must decompress under system zlib (windowBits 31)."
  (if (zlib-available-p)
      (loop for (name . maker) in +sample-datasets+
            do (let ((data (funcall maker 20000)))
                 (loop for level in '(0 1 6 9)
                       do (let* ((c (compress-octets data :format :gzip :level level))
                                 (d (z-gzip-inflate c)))
                            (is (equalp data d)
                                (format nil "cl->z gzip ~A level ~D" name level))))))
      (skip "system zlib not available")))

(test cross-z-gzip-cl-inflate
  "System zlib's gzip output must decompress under our gzip-decompress."
  (if (zlib-available-p)
      (loop for (name . maker) in +sample-datasets+
            do (let ((data (funcall maker 20000)))
                 (loop for level in '(0 1 6 9)
                       do (let* ((c (z-gzip-deflate data level))
                                 (d (decompress-octets c :format :gzip)))
                            (is (equalp data d)
                                (format nil "z->cl gzip ~A level ~D" name level))))))
      (skip "system zlib not available")))

(test cross-known-vectors
  "Our compressor must byte-match system zlib for the empty input, which has
  no freedom in block selection."
  (if (zlib-available-p)
      (let ((c (compress-octets (empty-data) :format :zlib :level 0)))
        (is (equalp (z-zlib-deflate (empty-data) 0) c)))
      (skip "system zlib not available")))

(test cross-window-boundary
  "Matches crossing the 32K window boundary decode correctly (both ways)."
  (if (zlib-available-p)
      (let* ((data (repetitive-data 70000))   ; > 2 * 32768, exercises window wrap
             (ours (compress-octets data :format :raw :level 6))
             (theirs (z-raw-deflate data 6)))
        (is (equalp data (z-raw-inflate ours)))
        (is (equalp data (decompress-octets theirs :format :raw))))
      (skip "system zlib not available")))
