(in-package #:cl-newzlib-tests)

(in-suite cl-newzlib-suite)

;;; ----------------------------------------------------------------
;;; Helpers
;;; ----------------------------------------------------------------

(defun concat-octets (&rest vectors)
  (let* ((n (reduce #'+ vectors :key #'length :initial-value 0))
         (out (make-array n :element-type '(unsigned-byte 8)))
         (pos 0))
    (declare (type fixnum n pos))
    (dolist (v vectors out)
      (replace out v :start1 pos)
      (incf pos (length v)))))

(defun slices-of (data size)
  "Split DATA into ranges of at most SIZE octets: list of (START END)."
  (loop for s from 0 below (length data) by size
        collect (list s (min (length data) (+ s size)))))

(defun state-compress-packets (data packet-size
                               &key (format :zlib) (level 6)
                                    (strategy :default) (flush :partial))
  "Compress DATA one packet at a time with DEFLATE-STATE.  Returns three
values: the list of per-packet compressed vectors, the corresponding list of
plaintext packets, and the final finish output (the trailer is only there)."
  (let ((state (cl-newzlib:make-deflate-state
                :format format :level level :strategy strategy))
        (packets '())
        (plain '()))
    (unwind-protect
         (progn
           (dolist (range (slices-of data packet-size))
             (destructuring-bind (start end) range
               (push (concat-octets
                      (cl-newzlib:deflate-state-write state data
                                                     :start start :end end)
                      (cl-newzlib:deflate-state-flush state :mode flush))
                     packets)
               (push (subseq data start end) plain)))
           (let ((tail (cl-newzlib:deflate-state-finish state)))
             (values (nreverse packets) (nreverse plain) tail)))
      (cl-newzlib:deflate-state-end state))))

(defun state-boundary-data (n)
  "Deterministic semi-compressible data.  At N around 4 KiB it compresses to a
dynamic block slightly larger than the 4096-octet initial writer buffer."
  (let ((v (make-array n :element-type '(unsigned-byte 8)))
        (x 1))
    (declare (type (unsigned-byte 32) x))
    (dotimes (i n v)
      (setf x (logand (+ (* x 1664525) 1013904223) #xFFFFFFFF))
      (setf (aref v i)
            (if (zerop (logand (ash x -24) 3))
                (logand (ash x -16) #xFF)
                (logand (+ (floor i 11) (floor (* i i) 1000)) 255))))))

(defun state-decompress-packets (packets &key (format :zlib) (tail nil))
  "Feed each compressed packet to a fresh INFLATE-STATE, then TAIL when
non-NIL.  Returns two values: the concatenated plaintext and whether the
stream finished."
  (let ((state (cl-newzlib:make-inflate-state :format format))
        (pieces '()))
    (unwind-protect
         (progn
           (dolist (packet packets)
             (push (cl-newzlib:inflate-state-write state packet) pieces))
           (when tail
             (push (cl-newzlib:inflate-state-write state tail) pieces))
           (values (apply #'concat-octets (nreverse pieces))
                   (cl-newzlib:inflate-state-finished-p state)))
      (cl-newzlib:inflate-state-end state))))

(defun state-roundtrip-one (data &key (format :zlib) (level 6)
                                  (strategy :default) (flush :partial)
                                  (packet-size 1000))
  (multiple-value-bind (packets plain tail)
      (state-compress-packets data packet-size
                              :format format :level level
                              :strategy strategy :flush flush)
    (let ((all (apply #'concat-octets (append packets (list tail)))))
      ;; One-shot decoder sees the finished stream.
      (is (equalp data (cl-newzlib:decompress-octets all :format format))
          (format nil "one-shot decode ~A L~D ~A ~A"
                  format level strategy flush))
      ;; Stateful decoder sees the exact same per-packet slices.
      (multiple-value-bind (decoded finished-p)
          (state-decompress-packets packets :format format :tail tail)
        (is (equalp data decoded)
            (format nil "stateful decode ~A L~D ~A ~A"
                    format level strategy flush))
        (is (eq t finished-p)
            (format nil "finished ~A L~D ~A ~A" format level strategy flush))
        (is (equalp data (apply #'concat-octets plain)))))))

;;; ----------------------------------------------------------------
;;; Self round trips
;;; ----------------------------------------------------------------

(test state-roundtrip-formats-levels-and-flush-modes
  (let ((data (repetitive-data 40000)))
    (dolist (format '(:zlib :gzip :raw))
      (dolist (level '(0 1 6 9))
        (dolist (flush '(:partial :sync))
          (state-roundtrip-one data :format format :level level :flush flush
                                    :packet-size 1500))))))

(test state-roundtrip-data-shapes
  (dolist (maker (list #'empty-data #'all-same-data #'text-data
                       #'incompressible-data #'repetitive-data))
    (let ((data (if (eq maker #'empty-data)
                    (funcall maker)
                    (funcall maker 12000))))
      (state-roundtrip-one data :format :zlib :level 6 :flush :sync
                                :packet-size 777))))

(test state-roundtrip-strategies
  (let ((data (repetitive-data 30000)))
    (dolist (strategy '(:default :huffman-only :fixed :rle))
      (state-roundtrip-one data :format :zlib :level 6 :strategy strategy
                                :flush :partial :packet-size 2048))))

(test state-roundtrip-arbitrary-sizes
  (dolist (n '(0 1 2 63 64 1023 1024 16383 16384 32767 32768 32769))
    (let ((data (if (zerop n) (empty-data) (repetitive-data n))))
      (state-roundtrip-one data :format :zlib :level 6 :flush :sync
                                :packet-size 333))))

(test state-roundtrip-one-mebibyte
  (let ((data (repetitive-data 1048576)))
    (state-roundtrip-one data :format :zlib :level 1 :flush :sync
                              :packet-size 50000)))

(test state-dynamic-block-writer-growth
  "Dynamic-block emission must grow the writer before a 4-octet word store.
A 2-octet wrapper header shifts word stores to offsets 2 mod 4, so a store at
4094 straddles a 4096-octet buffer; growing only when POS >= SIZE left a
two-octet hole in the compressed stream."
  (let* ((data (state-boundary-data 4321))
         (writer (cl-newzlib::make-bit-writer 4096)))
    ;; Emit a zlib header first, exactly as DEFLATE-STATE does for :ZLIB.
    (cl-newzlib::write-bits writer #x78 8)
    (cl-newzlib::write-bits writer #x01 8)
    (cl-newzlib::flush-pending-bytes writer)
    (cl-newzlib::deflate-into-writer data 0 (length data) writer 6)
    (cl-newzlib::flush-bits writer)
    (let* ((buffer (cl-newzlib::bw-buffer writer))
           (body (subseq buffer 2 (cl-newzlib::bw-pos writer))))
      (is (> (length body) 4094))
      ;; The growth path is only exercised by a dynamic block.
      (is (= 2 (ldb (byte 2 1) (aref body 0))))
      (is (equalp data (cl-newzlib:raw-inflate body))))))

(test state-inflate-state-tiny-chunks
  "A finished stream fed a few bytes at a time must decode incrementally and
only report finished after the trailer."
  (dolist (format '(:zlib :gzip :raw))
    (let* ((data (text-data 3000))
           (compressed (cl-newzlib:compress-octets data :format format))
           (state (cl-newzlib:make-inflate-state :format format))
           (pieces '()))
      (unwind-protect
           (progn
             (loop for start from 0 below (length compressed) by 3
                   do (push (cl-newzlib:inflate-state-write
                             state compressed :start start
                             :end (min (length compressed) (+ start 3)))
                            pieces)
                      (unless (= (min (length compressed) (+ start 3))
                                 (length compressed))
                        (is (not (cl-newzlib:inflate-state-finished-p state)))))
             (is (equalp data (apply #'concat-octets (nreverse pieces))))
             (is (cl-newzlib:inflate-state-finished-p state)))
        (cl-newzlib:inflate-state-end state)))))

;;; ----------------------------------------------------------------
;;; Cross-validation with system zlib, packet by packet
;;; ----------------------------------------------------------------

(test state-cl-to-zlib-many-packets
  "Packets flushed by DEFLATE-STATE must decompress one at a time under a
persistent C zlib inflater (OpenSSH's situation)."
  (if (zlib-available-p)
      (let ((data (repetitive-data 30000)))
        (dolist (format '(:zlib :gzip :raw))
          (dolist (level '(0 1 6 9))
            (dolist (flush '(:partial :sync))
              (multiple-value-bind (packets plain tail)
                  (state-compress-packets data 2000 :format format
                                          :level level :flush flush)
                (let* ((window-bits (case format
                                      (:raw -15) (:zlib 15) (:gzip 31)))
                       (inflater (make-z-inflate-stream
                                  :window-bits window-bits))
                       (decoded '()))
                  (unwind-protect
                       (progn
                         (loop for packet in packets
                               for expected in plain
                               do (let ((out (z-inflate-stream-feed
                                              inflater packet)))
                                    (is (equalp expected out)
                                        (format nil "per-packet ~A L~D ~A"
                                                format level flush))
                                    (push out decoded)))
                         ;; the finish output may carry pending data and the
                         ;; trailer; feed it so zlib reaches stream end
                         (let ((out (z-inflate-stream-feed inflater tail)))
                           (push out decoded))
                         (is (equalp data
                                     (apply #'concat-octets (nreverse decoded))))
                         (is (z-inflate-stream-finished inflater)))
                    (z-inflate-stream-end inflater))))))))
      (skip "system zlib not available")))

(test state-cl-to-zlib-strategies
  (if (zlib-available-p)
      (let ((data (repetitive-data 20000)))
        (dolist (strategy '(:huffman-only :rle :fixed))
          (multiple-value-bind (packets plain tail)
              (state-compress-packets data 2500 :format :zlib :level 6
                                      :strategy strategy :flush :sync)
            (let ((inflater (make-z-inflate-stream :window-bits 15)))
              (unwind-protect
                   (let ((decoded '()))
                     (loop for packet in packets
                           for expected in plain
                           do (let ((out (z-inflate-stream-feed inflater packet)))
                                (is (equalp expected out)
                                    (format nil "strategy ~A packet" strategy))
                                (push out decoded)))
                     (push (z-inflate-stream-feed inflater tail) decoded)
                     (is (equalp data (apply #'concat-octets (nreverse decoded))))
                     (is (z-inflate-stream-finished inflater)))
                (z-inflate-stream-end inflater))))))
      (skip "system zlib not available")))

(test state-zlib-to-cl-many-packets
  "Packets flushed by C zlib with Z_PARTIAL_FLUSH / Z_SYNC_FLUSH must decode
one at a time through INFLATE-STATE."
  (if (zlib-available-p)
      (let ((data (text-data 30000)))
        (dolist (format '(:zlib :gzip :raw))
          (dolist (level '(0 1 6 9))
            (dolist (flush (list +z-partial-flush+ +z-sync-flush+))
              (let* ((window-bits (case format
                                    (:raw -15) (:zlib 15) (:gzip 31)))
                     (deflater (make-z-deflate-stream
                                :window-bits window-bits :level level))
                     (state (cl-newzlib:make-inflate-state :format format))
                     (decoded '()))
                (unwind-protect
                     (progn
                       (dolist (range (slices-of data 2000))
                         (destructuring-bind (start end) range
                           (let* ((packet (z-deflate-stream-flush
                                           deflater (subseq data start end) flush))
                                  (out (cl-newzlib:inflate-state-write
                                        state packet)))
                             (is (equalp (subseq data start end) out)
                                 (format nil "z->cl per-packet ~A L~D ~A"
                                         format level flush))
                             (push out decoded))))
                       (push (cl-newzlib:inflate-state-write
                              state (z-deflate-stream-flush
                                     deflater (empty-data) +z-finish+))
                             decoded)
                       (is (equalp data (apply #'concat-octets (nreverse decoded))))
                       (is (cl-newzlib:inflate-state-finished-p state)))
                  (z-deflate-stream-end deflater)
                  (cl-newzlib:inflate-state-end state)))))))
      (skip "system zlib not available")))

;;; ----------------------------------------------------------------
;;; Wrapper details
;;; ----------------------------------------------------------------

(test state-reset-starts-a-new-stream
  (dolist (format '(:zlib :gzip :raw))
    (let* ((a (repetitive-data 20000))
           (b (text-data 20000))
           (state (cl-newzlib:make-deflate-state :format format :level 6))
           (parts-a (list (cl-newzlib:deflate-state-write state a)
                          (cl-newzlib:deflate-state-flush state :mode :sync)))
           (header (cl-newzlib:deflate-state-reset state))
           (parts-b (list (cl-newzlib:deflate-state-write state b)
                          (cl-newzlib:deflate-state-flush state :mode :sync)
                          (cl-newzlib:deflate-state-finish state)))
           (expected-header (case format (:zlib 2) (:gzip 10) (t 0))))
      (is (= expected-header (length header))
          (format nil "reset header length ~A" format))
      (let ((decoder (cl-newzlib:make-inflate-state :format format)))
        (is (equalp a (cl-newzlib:inflate-state-write
                       decoder (apply #'concat-octets parts-a))))
        (is (not (cl-newzlib:inflate-state-finished-p decoder)))
        (cl-newzlib:inflate-state-reset decoder)
        (let ((out (apply #'concat-octets
                          (cl-newzlib:inflate-state-write decoder header)
                          (mapcar (lambda (p)
                                    (cl-newzlib:inflate-state-write decoder p))
                                  parts-b))))
          (is (equalp b out) (format nil "post-reset payload ~A" format))
          (is (cl-newzlib:inflate-state-finished-p decoder)))
        (cl-newzlib:inflate-state-end decoder))
      (cl-newzlib:deflate-state-end state))))

(test state-zlib-adler-trailer
  (let* ((data (text-data 5000))
         (state (cl-newzlib:make-deflate-state :format :zlib :level 6))
         (compressed (concat-octets
                      (cl-newzlib:deflate-state-write state data)
                      (cl-newzlib:deflate-state-flush state :mode :sync)
                      (cl-newzlib:deflate-state-finish state)))
         (without-trailer (subseq compressed 0 (- (length compressed) 4)))
         (decoder (cl-newzlib:make-inflate-state :format :zlib)))
    ;; All the payload, no trailer yet.
    (is (equalp data (cl-newzlib:inflate-state-write decoder without-trailer)))
    (is (not (cl-newzlib:inflate-state-finished-p decoder)))
    ;; The trailer alone finishes the stream.
    (is (zerop (length (cl-newzlib:inflate-state-write
                        decoder compressed :start (- (length compressed) 4)))))
    (is (cl-newzlib:inflate-state-finished-p decoder))
    ;; A corrupt Adler-32 is a format error.
    (let ((bad (copy-seq compressed)))
      (setf (aref bad (1- (length bad)))
            (logxor 1 (aref bad (1- (length bad)))))
      (signals newzlib-format-error
        (cl-newzlib:inflate-state-write
         (cl-newzlib:make-inflate-state :format :zlib) bad)))
    (cl-newzlib:inflate-state-end decoder)
    (cl-newzlib:deflate-state-end state)))

(test state-gzip-trailer
  (let* ((data (repetitive-data 6000))
         (state (cl-newzlib:make-deflate-state :format :gzip :level 1))
         (compressed (concat-octets
                      (cl-newzlib:deflate-state-write state data)
                      (cl-newzlib:deflate-state-finish state)))
         (decoder (cl-newzlib:make-inflate-state :format :gzip)))
    (is (equalp data (cl-newzlib:inflate-state-write
                      decoder compressed :end (- (length compressed) 8))))
    (is (not (cl-newzlib:inflate-state-finished-p decoder)))
    (is (zerop (length (cl-newzlib:inflate-state-write
                        decoder compressed :start (- (length compressed) 8)))))
    (is (cl-newzlib:inflate-state-finished-p decoder))
    (let ((bad (copy-seq compressed)))
      (setf (aref bad (- (length bad) 8))
            (logxor 1 (aref bad (- (length bad) 8))))
      (signals newzlib-format-error
        (cl-newzlib:inflate-state-write
         (cl-newzlib:make-inflate-state :format :gzip) bad)))
    (cl-newzlib:inflate-state-end decoder)
    (cl-newzlib:deflate-state-end state)))

(test state-raw-has-no-trailer
  (let* ((data (repetitive-data 4000))
         (state (cl-newzlib:make-deflate-state :format :raw :level 6))
         (compressed (concat-octets
                      (cl-newzlib:deflate-state-write state data)
                      (cl-newzlib:deflate-state-flush state :mode :sync)
                      (cl-newzlib:deflate-state-finish state)))
         (decoder (cl-newzlib:make-inflate-state :format :raw)))
    (is (equalp data (cl-newzlib:inflate-state-write decoder compressed)))
    (is (cl-newzlib:inflate-state-finished-p decoder))
    (cl-newzlib:inflate-state-end decoder)
    (cl-newzlib:deflate-state-end state)))

;;; ----------------------------------------------------------------
;;; Edge cases
;;; ----------------------------------------------------------------

(test state-empty-flush-is-a-valid-boundary
  (dolist (format '(:zlib :gzip :raw))
    (dolist (flush '(:partial :sync))
      (let* ((state (cl-newzlib:make-deflate-state :format format :level 6))
             (compressed (concat-octets
                          (cl-newzlib:deflate-state-flush state :mode flush)
                          (cl-newzlib:deflate-state-flush state :mode flush)
                          (cl-newzlib:deflate-state-write state (empty-data))
                          (cl-newzlib:deflate-state-flush state :mode flush)
                          (cl-newzlib:deflate-state-finish state))))
        (is (equalp (empty-data)
                    (cl-newzlib:decompress-octets compressed :format format))
            (format nil "empty flush boundary ~A ~A" format flush))
        (cl-newzlib:deflate-state-end state)))))

(test state-write-after-finish-signals
  (let ((state (cl-newzlib:make-deflate-state :format :zlib :level 6))
        (data (text-data 100)))
    (cl-newzlib:deflate-state-finish state)
    (signals newzlib-parameter-error
      (cl-newzlib:deflate-state-write state data))
    (signals newzlib-parameter-error
      (cl-newzlib:deflate-state-flush state))
    ;; Double finish is idempotent.
    (is (zerop (length (cl-newzlib:deflate-state-finish state))))
    ;; Reset revives the object with a fresh header.
    (let ((header (cl-newzlib:deflate-state-reset state)))
      (is (= 2 (length header)))
      (let ((compressed (concat-octets
                         header
                         (cl-newzlib:deflate-state-write state data)
                         (cl-newzlib:deflate-state-finish state))))
        (is (equalp data (cl-newzlib:decompress-octets compressed :format :zlib)))))
    (cl-newzlib:deflate-state-end state)))

(test state-inflate-truncated-mid-block-is-not-an-error
  (let* ((data (repetitive-data 2000))
         (compressed (cl-newzlib:compress-octets data :format :zlib))
         (state (cl-newzlib:make-inflate-state :format :zlib)))
    ;; Every proper prefix must be accepted (returning whatever is available)
    ;; and must not report finished.
    (loop for n from 0 below (length compressed)
          do (let ((fresh (cl-newzlib:make-inflate-state :format :zlib)))
               (cl-newzlib:inflate-state-write fresh compressed :end n)
               (is (not (cl-newzlib:inflate-state-finished-p fresh)))
               (cl-newzlib:inflate-state-end fresh)))
    (is (equalp data (cl-newzlib:inflate-state-write state compressed)))
    (is (cl-newzlib:inflate-state-finished-p state))
    (cl-newzlib:inflate-state-end state)))

(test state-many-small-packets-stay-bounded
  (let ((state (cl-newzlib:make-deflate-state :format :zlib :level 1))
        (initial-tokens 0)
        (max-data 0)
        (max-pos 0)
        (max-tokens 0)
        (packets '()))
    (unwind-protect
         (progn
           (setf initial-tokens
                 (cl-newzlib::lzs-token-capacity
                  (cl-newzlib::dst-scratch state)))
           (dotimes (i 10000)
             (let ((packet (make-array 8 :element-type '(unsigned-byte 8)
                                         :initial-element (mod i 251))))
               (push (concat-octets
                      (cl-newzlib:deflate-state-write state packet)
                      (cl-newzlib:deflate-state-flush state :mode :sync))
                     packets)
               (setf max-data (max max-data (length (cl-newzlib::dst-data state)))
                     max-pos (max max-pos (cl-newzlib::dst-data-pos state))
                     max-tokens (max max-tokens
                                     (cl-newzlib::lzs-token-capacity
                                      (cl-newzlib::dst-scratch state))))))
           ;; 80 KB of input must not accumulate: the window slides.
           (is (< max-pos 131072) (format nil "max window position ~D" max-pos))
           (is (< max-data 200000) (format nil "max window buffer ~D" max-data))
           (is (<= max-tokens initial-tokens)
               (format nil "token capacity grew from ~D to ~D"
                       initial-tokens max-tokens))
           (let ((compressed (concat-octets
                              (apply #'concat-octets (nreverse packets))
                              (cl-newzlib:deflate-state-finish state)))
                 (expected (make-array (* 8 10000)
                                       :element-type '(unsigned-byte 8))))
             (dotimes (i 10000)
               (fill expected (mod i 251) :start (* 8 i) :end (* 8 (1+ i))))
             (is (equalp expected
                         (cl-newzlib:decompress-octets compressed :format :zlib)))))
      (cl-newzlib:deflate-state-end state))))

(test state-refuses-ended-objects
  (let ((state (cl-newzlib:make-deflate-state :format :raw)))
    (cl-newzlib:deflate-state-end state)
    (signals newzlib-parameter-error
      (cl-newzlib:deflate-state-write state (text-data 10)))
    (signals newzlib-parameter-error
      (cl-newzlib:deflate-state-finish state)))
  (let ((state (cl-newzlib:make-inflate-state :format :raw)))
    (cl-newzlib:inflate-state-end state)
    (signals newzlib-parameter-error
      (cl-newzlib:inflate-state-write state (text-data 10)))))

(test state-rejects-invalid-parameters
  (signals newzlib-parameter-error
    (cl-newzlib:make-deflate-state :format :brotli))
  (signals newzlib-parameter-error
    (cl-newzlib:make-deflate-state :level 10))
  (signals newzlib-parameter-error
    (cl-newzlib:make-deflate-state :level -1))
  (signals newzlib-parameter-error
    (cl-newzlib:make-deflate-state :strategy :fast))
  (signals newzlib-parameter-error
    (cl-newzlib:make-deflate-state :window-bits 7))
  (signals newzlib-parameter-error
    (cl-newzlib:make-deflate-state :window-bits 16))
  (signals newzlib-parameter-error
    (cl-newzlib:make-deflate-state :mem-level 0))
  (signals newzlib-parameter-error
    (cl-newzlib:make-deflate-state :mem-level 10))
  (signals newzlib-parameter-error
    (cl-newzlib:make-inflate-state :format :brotli))
  (signals newzlib-parameter-error
    (cl-newzlib:make-inflate-state :window-bits 16))
  (let ((state (cl-newzlib:make-deflate-state :format :raw)))
    (unwind-protect
         (progn
           (signals newzlib-parameter-error
             (cl-newzlib:deflate-state-flush state :mode :full))
           (signals newzlib-parameter-error
             (cl-newzlib:deflate-state-write state (text-data 10) :start 5 :end 2))
           (signals newzlib-parameter-error
             (cl-newzlib:deflate-state-write state (text-data 10) :start 0 :end 11))
           (signals newzlib-parameter-error
             (cl-newzlib:deflate-state-write state "not octets")))
      (cl-newzlib:deflate-state-end state))))

(test state-inflate-rejects-corrupt-streams
  ;; BTYPE = 11 is reserved in DEFLATE.
  (signals newzlib-format-error
    (cl-newzlib:inflate-state-write
     (cl-newzlib:make-inflate-state :format :raw)
     (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(6))))
  ;; CM = 0 in the zlib header is not DEFLATE.
  (signals newzlib-format-error
    (cl-newzlib:inflate-state-write
     (cl-newzlib:make-inflate-state :format :zlib)
     (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(0 0))))
  ;; CMF/FLG must satisfy (CMF*256+FLG) mod 31 = 0; 0x78 0x00 does not.
  (signals newzlib-format-error
    (cl-newzlib:inflate-state-write
     (cl-newzlib:make-inflate-state :format :zlib)
     (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(#x78 0))))
  ;; Gzip magic is checked incrementally.
  (signals newzlib-format-error
    (cl-newzlib:inflate-state-write
     (cl-newzlib:make-inflate-state :format :gzip)
     (text-data 12))))

(test state-inflate-reset-discards-history
  (let* ((a (repetitive-data 5000))
         (b (text-data 5000))
         (compressed-a (cl-newzlib:compress-octets a :format :raw))
         (compressed-b (cl-newzlib:compress-octets b :format :raw))
         (state (cl-newzlib:make-inflate-state :format :raw)))
    (unwind-protect
         (progn
           ;; Consume part of A, leaving a half-decoded block buffered.
           (cl-newzlib:inflate-state-write state compressed-a :end 40)
           (cl-newzlib:inflate-state-reset state)
           (is (equalp b (cl-newzlib:inflate-state-write state compressed-b)))
           (is (cl-newzlib:inflate-state-finished-p state)))
      (cl-newzlib:inflate-state-end state))))

(test state-packet-helpers
  (dolist (flush '(:partial :sync))
    (let* ((data (text-data 9000))
           (comp (cl-newzlib:make-deflate-state
                  :format :zlib :level 1 :strategy :huffman-only))
           (dec (cl-newzlib:make-inflate-state :format :zlib)))
      (unwind-protect
           (progn
             (dolist (range (slices-of data 700))
               (destructuring-bind (start end) range
                 (let ((packet (cl-newzlib:deflate-packet
                                comp data :start start :end end :mode flush)))
                   (is (plusp (length packet)))
                   (is (equalp (subseq data start end)
                               (cl-newzlib:inflate-packet dec packet))))))
             (let ((tail (cl-newzlib:deflate-state-finish comp)))
               (is (zerop (length (cl-newzlib:inflate-state-write dec tail))))
               (is (cl-newzlib:inflate-state-finished-p dec))))
        (cl-newzlib:inflate-state-end dec)
        (cl-newzlib:deflate-state-end comp)))))

(test state-tiny-packet-sizes
  (dolist (packet-size '(1 7 32))
    (let ((data (text-data 2000)))
      (state-roundtrip-one data :format :zlib :level 1
                                :strategy :huffman-only
                                :flush :partial
                                :packet-size packet-size))))

(test state-huffman-only-levels
  ;; OpenSSH 10.6 uses level 1 + Z_HUFFMAN_ONLY; accept other levels too.
  (dolist (level '(1 6 9))
    (state-roundtrip-one (text-data 8000) :format :zlib :level level
                                              :strategy :huffman-only
                                              :flush :partial
                                              :packet-size 512)))

(test state-inflate-many-small-packets-stay-bounded
  (let ((comp (cl-newzlib:make-deflate-state
               :format :zlib :level 1 :strategy :huffman-only))
        (dec (cl-newzlib:make-inflate-state :format :zlib))
        (max-in 0)
        (max-out 0))
    (unwind-protect
         (progn
           (dotimes (i 10000)
             (let ((packet (make-array 16 :element-type '(unsigned-byte 8)
                                          :initial-element (mod i 251))))
               (let* ((compressed (cl-newzlib:deflate-packet
                                   comp packet :mode :partial))
                      (plain (cl-newzlib:inflate-packet dec compressed)))
                 (unless (equalp packet plain)
                   (error "packet ~D round trip mismatch" i))))
             (setf max-in (max max-in (cl-newzlib::ist-in-size dec))
                   max-out (max max-out (cl-newzlib::ist-out-size dec))))
           (cl-newzlib:inflate-state-write dec (cl-newzlib:deflate-state-finish comp))
           (is (cl-newzlib:inflate-state-finished-p dec))
           ;; Compressed input consumed per packet is tiny, and the decoded
           ;; history is compacted to one window, so neither buffer grows with
           ;; the session length.
           (is (<= max-in 8192) (format nil "max input buffer ~D" max-in))
           (is (<= max-out (* 4 65536)) (format nil "max output buffer ~D" max-out)))
      (cl-newzlib:inflate-state-end dec)
      (cl-newzlib:deflate-state-end comp))))
