(in-package #:cl-newzlib-tests)

(in-suite cl-newzlib-suite)

;;; ----------------------------------------------------------------
;;; Deterministic sample data
;;; ----------------------------------------------------------------

(defun empty-data (&optional size)
  (declare (ignore size))
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun small-data (&optional size)
  (declare (ignore size))
  (map '(vector (unsigned-byte 8)) #'char-code
       "The quick brown fox jumps over the lazy dog. 0123456789!"))

(defun repetitive-data (n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (logand (floor i 37) 255)))))

(defun all-same-data (n)
  (let ((v (make-array n :element-type '(unsigned-byte 8) :initial-element 65)))
    v))

(defun text-data (n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i)
            (logand (char-code (aref "The quick brown fox jumps over the lazy dog. "
                                     (mod i 45)))
                    255)))))

(defun incompressible-data (n)
  (random-octets n))

(defparameter +sample-datasets+
  '(("empty" . empty-data)
    ("small" . small-data)
    ("repetitive" . repetitive-data)
    ("all-same" . all-same-data)
    ("text" . text-data)
    ("random" . incompressible-data)))

(test roundtrip-all-formats-all-levels
  (loop for (name . maker) in +sample-datasets+
        for data = (funcall maker 20000)
        do (loop for format in '(:zlib :gzip :raw)
                 do (loop for level in '(0 1 6 9)
                          do (let* ((c (compress-octets data :format format :level level))
                                    (d (decompress-octets c :format format)))
                               (is (equalp data d)
                                   (format nil "roundtrip ~A level ~D ~A" name level format)))))))

(test fast-mode-roundtrip-all-formats
  (loop for format in '(:raw :zlib :gzip)
        for level in '(1 6 9)
        do (let* ((data (repetitive-data 20000))
                  (compressed (compress-octets data :format format
                                                 :level level :mode :fast))
                  (capacity (+ (length data) (ash (length data) -3) 300))
                  (output (make-array capacity :element-type '(unsigned-byte 8)
                                       :initial-element #xA5))
                  (count (compress-into output data :format format
                                         :level level :mode :fast)))
           (is (= count (length compressed)))
           (is (equalp compressed (subseq output 0 count)))
           (is (equalp data (decompress-octets compressed :format format)))
           (is (= #xA5 (aref output count))))))

(test paired-decode-text-roundtrip
  (let* ((data (text-data 20000))
         (compressed (compress-octets data :format :raw :level 6))
         (output (make-array (1+ (length data)) :element-type '(unsigned-byte 8)
                             :initial-element #xA5)))
    (let ((count (decompress-into output compressed :format :raw)))
      (is (= count (length data)))
      (is (equalp data (subseq output 0 count)))
      (is (= #xA5 (aref output count))))))

(test compress-into-reusable-output
  (loop for format in '(:raw :zlib :gzip)
        for level in '(0 1 6 9)
        do (let* ((data (incompressible-data 20000))
                  (capacity (+ (length data) (ash (length data) -3) 300))
                  (output (make-array capacity :element-type '(unsigned-byte 8)
                                       :initial-element #xA5))
                  (count (compress-into output data :format format :level level))
                  (expected (compress-octets data :format format :level level)))
             (is (= count (length expected)))
             (is (equalp expected (subseq output 0 count)))
             (is (= #xA5 (aref output count)))
             (is (equalp data (decompress-octets (subseq output 0 count)
                                                 :format format)))
             (let ((second-count (compress-into output data :format format
                                                :level level)))
               (is (= second-count count))
               (is (equalp expected (subseq output 0 count)))))))

(test compress-into-capacity-and-alias
  (let ((data (incompressible-data 1000)))
    (signals newzlib-parameter-error
      (compress-into (make-array 1 :element-type '(unsigned-byte 8)) data))
    (signals newzlib-parameter-error
      (compress-into data data))))

(test compress-into-large-stored
  (let* ((data (incompressible-data 200000))
         (capacity (+ (length data) (ash (length data) -3) 300))
         (output (make-array capacity :element-type '(unsigned-byte 8)
                              :initial-element #xA5)))
    (dolist (format '(:raw :zlib :gzip))
      (let* ((count (compress-into output data :format format :level 0))
             (expected (compress-octets data :format format :level 0)))
        (is (= count (length expected)))
        (is (equalp expected (subseq output 0 count)))
        (is (equalp data (decompress-octets (subseq output 0 count)
                                            :format format)))))))

(test decompress-into-reusable-output
  (loop for format in '(:raw :zlib :gzip)
        for level in '(0 1 6 9)
        do (let* ((data (incompressible-data 20000))
                  (compressed (compress-octets data :format format :level level))
                  (output (make-array (+ (length data) 300)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element #xA5))
                  (count (decompress-into output compressed :format format)))
             (is (= count (length data)))
             (is (equalp data (subseq output 0 count)))
             (is (= #xA5 (aref output count)))
             (let ((second-count (decompress-into output compressed :format format)))
               (is (= second-count count))
               (is (equalp data (subseq output 0 count)))))))

(test decompress-into-capacity-and-alias
  (let* ((data (incompressible-data 1000))
         (compressed (compress-octets data :format :zlib)))
    (signals newzlib-parameter-error
      (decompress-into (make-array 1 :element-type '(unsigned-byte 8))
                       compressed))
    (signals newzlib-parameter-error
      (decompress-into compressed compressed))))

(test roundtrip-empty
  (loop for format in '(:zlib :gzip :raw)
        do (let* ((c (compress-octets (empty-data) :format format))
                  (d (decompress-octets c :format format)))
             (is (= 0 (length d)))
             (is (equalp (empty-data) d)))))

(test roundtrip-tiny
  (loop for n in '(1 2 3 4 100 255 256 257 258)
        do (loop for level in '(0 1 6 9)
                 do (let* ((data (all-same-data n))
                           (c (compress-octets data :format :raw :level level))
                           (d (decompress-octets c :format :raw)))
                      (is (equalp data d)
                          (format nil "tiny n=~D level=~D" n level))))))

(test stored-blocks-large-incompressible
  ;; incompressible data larger than one stored block (65535 bytes) must
  ;; roundtrip; the stored fallback has to split into multiple blocks
  (loop for level in '(0 1 6 9)
        do (let* ((data (incompressible-data 300000))
                  (c (compress-octets data :format :raw :level level))
                  (d (decompress-octets c :format :raw)))
             (is (equalp data d)
                 (format nil "stored fallback level ~D" level)))))

(test stored-blocks-large-incompressible-zlib
  ;; same, through the zlib wrapper so the Adler-32 trailer is checked
  (let* ((data (incompressible-data 200000))
         (c (compress-octets data :format :zlib :level 1))
         (d (decompress-octets c :format :zlib)))
    (is (equalp data d))))

(test compress-level-0-is-stored
  (let* ((data (incompressible-data 5000))
         (c (compress-octets data :format :raw :level 0))
         (d (decompress-octets c :format :raw)))
    (is (equalp data d))
    ;; level 0 must use stored blocks: output >= input
    (is (>= (length c) (length data)))))

(test format-dispatch-errors
  (signals cl-newzlib:newzlib-parameter-error
    (compress-octets (small-data) :format :bogus))
  (signals cl-newzlib:newzlib-parameter-error
    (compress-octets (small-data) :mode :bogus))
  (signals cl-newzlib:newzlib-parameter-error
    (decompress-octets (small-data) :format :bogus))
  (signals cl-newzlib:newzlib-parameter-error
    (compress-octets (small-data) :format :zlib :level 12)))

(test corrupt-input-rejected
  (let* ((data (small-data))
         (c (compress-octets data :format :raw))
         (bad (copy-seq c)))
    (when (plusp (length bad))
      (setf (aref bad 0) (logxor (aref bad 0) #xFF)))
    (signals cl-newzlib:newzlib-format-error
      (decompress-octets bad :format :raw))))

(test truncated-input-rejected
  (let* ((data (incompressible-data 1000))
         (c (compress-octets data :format :raw)))
    ;; truncated input may surface as either a format error or end-of-input;
    ;; both derive from NEWZLIB-ERROR
    (signals cl-newzlib:newzlib-error
      (decompress-octets (subseq c 0 (max 1 (floor (length c) 2)))
                         :format :raw))))

(test zlib-header-invalid-rejected
  (let* ((data (small-data))
         (c (compress-octets data :format :zlib)))
    (setf (aref c 0) (logxor (aref c 0) #xFF))  ; corrupt CMF
    (signals cl-newzlib:newzlib-format-error
      (decompress-octets c :format :zlib))))

(test adler-checksum-validated
  (let* ((data (incompressible-data 2000))
         (c (compress-octets data :format :zlib)))
    (setf (aref c (1- (length c))) (logxor (aref c (1- (length c))) #xFF))
    (signals cl-newzlib:newzlib-format-error
      (decompress-octets c :format :zlib))))

;;; ----------------------------------------------------------------
;;; Streaming API
;;; ----------------------------------------------------------------

(test deflate-stream-roundtrip
  (let* ((data (repetitive-data 100000))
         (ds (make-deflate-stream :level 6)))
    (deflate-stream-write ds data 0 50000)
    (deflate-stream-write ds data 50000 100000)
    (let* ((c (deflate-stream-finish ds))
           (d (decompress-octets c :format :raw)))
      (is (equalp data d))
      (deflate-stream-end ds))))

(test deflate-stream-fast-mode
  (let* ((data (repetitive-data 100000))
         (ds (make-deflate-stream :level 6 :mode :fast)))
    (deflate-stream-write ds data)
    (let ((c (deflate-stream-finish ds)))
      (is (equalp data (decompress-octets c :format :raw))))
    (deflate-stream-end ds)))
(test deflate-stream-empty
  (let* ((ds (make-deflate-stream))
         (c (deflate-stream-finish ds)))
    (is (plusp (length c)))
    (is (= 0 (length (decompress-octets c :format :raw))))
    (deflate-stream-end ds)))

(test inflate-stream-chunked
  (let* ((data (repetitive-data 100000))
         (c (compress-octets data :format :raw :level 6))
         (is (make-inflate-stream c))
         (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t
                          :fill-pointer t)))
    (loop until (inflate-stream-eof-p is)
          do (let ((chunk (inflate-stream-read is 1000)))
               (loop for i below (length chunk)
                     do (vector-push-extend (aref chunk i) out))))
    (is (equalp data (subseq out 0 (length out))))
    (is (= 100000 (length out)))
    (inflate-stream-end is)))

(test inflate-stream-short-input
  (let* ((data (small-data))
         (c (compress-octets data :format :raw))
         (is (make-inflate-stream c)))
    (let ((chunk (inflate-stream-read is (+ (length data) 10))))
      (is (= (length data) (length chunk)))
      (is (equalp data chunk))
      (is (inflate-stream-eof-p is)))
    (inflate-stream-end is)))

;;; ----------------------------------------------------------------
;;; Pathname / stream convenience
;;; ----------------------------------------------------------------

(test compress-decompress-files
  (let* ((data (text-data 5000))
         (tag (gensym))
         (in-file (make-pathname :name (format nil "clz-test-in-~A" tag)
                                 :type "bin" :defaults #p"/tmp/"))
         (gz-file (make-pathname :name (format nil "clz-test-in-~A" tag)
                                 :type "gz" :defaults #p"/tmp/")))
    (unwind-protect
         (progn
           (with-open-file (s in-file :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
             (write-sequence data s))
           (let ((c (compress in-file :format :gzip)))
             (with-open-file (s gz-file :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
               (write-sequence c s)))
           (is (equalp data (decompress gz-file :format :gzip))))
      (when (probe-file in-file) (delete-file in-file))
      (when (probe-file gz-file) (delete-file gz-file)))))

(test compress-decompress-streams
  (let* ((data (text-data 5000))
         (tag (gensym))
         (in-file (make-pathname :name (format nil "clz-test-stream-~A" tag)
                                 :type "bin" :defaults #p"/tmp/"))
         (gz-file (make-pathname :name (format nil "clz-test-stream-~A" tag)
                                 :type "gz" :defaults #p"/tmp/")))
    (unwind-protect
         (progn
           (with-open-file (s in-file :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
             (write-sequence data s))
           ;; pass the binary input stream, not the pathname
           (let ((c (with-open-file (s in-file :direction :input
                                       :element-type '(unsigned-byte 8))
                      (compress s :format :gzip))))
             (with-open-file (s gz-file :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
               (write-sequence c s)))
           (is (equalp data (with-open-file (s gz-file :direction :input
                                               :element-type '(unsigned-byte 8))
                              (decompress s :format :gzip)))))
      (when (probe-file in-file) (delete-file in-file))
      (when (probe-file gz-file) (delete-file gz-file)))))

;;; ------------------------------------------------------------------
;;; Regression: unsynchronized lazy initialization of the fixed Huffman
;;; tables and static trees was a data race (a thread entering between the
;;; two SETFs of ENSURE-FIXED-TABLES received a NIL distance table and, at
;;; safety 0, faulted).  Both are now built eagerly at load time.
;;; ------------------------------------------------------------------

(test fixed-tables-eagerly-built
  "The fixed decode tables and static trees must exist immediately after
load -- no lazy initialization left to race."
  (is (not (null cl-newzlib::+fixed-lit-table+)))
  (is (not (null cl-newzlib::+fixed-dist-table+)))
  (is (not (null cl-newzlib::+static-lit-codes+)))
  (is (not (null cl-newzlib::+static-lit-lengths+)))
  (is (not (null cl-newzlib::+static-dist-codes+)))
  (is (not (null cl-newzlib::+static-dist-lengths+))))

#+sb-thread
(test concurrent-inflate-stress
  "Eight threads inflating fixed-Huffman and mixed streams concurrently.
Reproduces the conditions of the lazy-init race (which is now impossible
by construction); also shakes out any other inflate state sharing."
  (let* ((data (random-octets 2048))
         (empty (make-array 0 :element-type '(unsigned-byte 8)))
         ;; an empty stream always emits a fixed-Huffman block; small
         ;; level-1 streams mix fixed/dynamic/stored blocks
         (jobs (append (make-list 64 :initial-element
                                     (cons (raw-deflate empty 0 0 1) empty))
                       (loop for i below 128
                             for len = (* 4 (1+ (mod i 512)))
                             collect (cons (raw-deflate data 0 len 1)
                                           (subseq data 0 len)))))
         (n (length jobs))
         (failures '())
         (lock (sb-thread:make-mutex :name "inflate-stress"))
         (threads (loop for w below 8
                        collect (sb-thread:make-thread
                                 (lambda ()
                                   (loop for j from w below n by 8
                                         do (destructuring-bind (c . orig)
                                                (nth j jobs)
                                              (unless (equalp (raw-inflate c) orig)
                                                (sb-thread:with-mutex (lock)
                                                  (push j failures))))))))))
    (dolist (th threads) (sb-thread:join-thread th))
    (is (null failures))))
