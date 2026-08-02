(in-package #:cl-newzlib)

;;; DEFLATE compressor (RFC 1951).
;;;
;;; Pipeline: LZ77 match finding over the input with a 32 KiB sliding window
;;; (hash chains), token stream in literal/length-distance form, then block
;;; emission.  For each block we build Huffman codes from the token
;;; frequencies and pick the cheapest of stored / fixed / dynamic encoding.

(defconstant +window-size+ 32768)
(defconstant +hash-size+ 32768)
(defconstant +window-mask+ (1- +window-size+))
(defconstant +hash-mask+ (1- +hash-size+))
(defconstant +min-match+ 3)
(defconstant +max-match+ 258)
(defconstant +max-dist+ 32768)
(defconstant +max-stored-block+ 65535)

;;; Compression-level tuning (approximates zlib's max_chain_length and
;;; nice_length for each level; we do greedy matching, no lazy).
(defun chain-limit (level)
  (case level
    (0 0)
    ((1 2) 4)
    ((3 4) 32)
    ((5 6) 128)
    ((7 8) 512)
    (9 4096)
    (otherwise (error 'newzlib-parameter-error
                      :detail (format nil "invalid compression level ~A" level)))))

(defun nice-length (level)
  (case level
    (0 0)
    ((1 2) 8)
    ((3 4) 32)
    ((5 6) 128)
    ((7 8) 128)
    (9 258)
    (otherwise (error 'newzlib-parameter-error
                      :detail (format nil "invalid compression level ~A" level)))))

(declaim (inline hash-3))
(defun hash-3 (input pos)
  "Hash the three bytes at INPUT[POS..POS+2] into HASH-SIZE buckets."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array input)
           (type fixnum pos))
  (logand (logxor (aref input pos)
                  (ash (aref input (1+ pos)) 8)
                  (ash (aref input (+ pos 2)) 16))
          +hash-mask+))

(declaim (inline insert-string))
(defun insert-string (input pos head prev)
  "Insert POS into the hash chain for its 3-byte hash."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array input head prev)
           (type fixnum pos))
  (let ((h (hash-3 input pos)))
    (setf (aref prev (logand pos +window-mask+)) (aref head h)
          (aref head h) pos)))

(defun longest-match (input pos end head prev max-chain nice)
  "Find the longest match for the string starting at INPUT[POS].  Returns
(VALUES LENGTH DISTANCE), both 0 when no match of at least MIN-MATCH exists."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array input head prev)
           (type fixnum pos end max-chain nice))
  (let ((best-len 0)
        (best-dist 0)
        (chain 0)
        (limit (max 0 (- pos +max-dist+))))
    (declare (type fixnum best-len best-dist chain limit))
    (let ((h (hash-3 input pos)))
      (loop for cand = (aref head h) then (aref prev (logand cand +window-mask+)) do
        (when (or (minusp cand) (< cand limit)) (return))
        (when (>= chain max-chain) (return))
        (incf chain)
        (let ((len 0))
          (declare (type fixnum len))
          (loop while (and (< (+ pos len) end)
                           (< (+ cand len) pos)
                           (< len +max-match+)
                           (= (aref input (+ cand len)) (aref input (+ pos len))))
                do (incf len))
          (when (> len best-len)
            (setf best-len len
                  best-dist (- pos cand))
            (when (>= len nice) (return)))))
      (values best-len best-dist))))

;;; ------------------------------------------------------------------
;;; LZ77 tokenization
;;; ------------------------------------------------------------------
;;;
;;; Token representation (parallel arrays):
;;;   sym[i]  : literal byte (0..255), end-of-block (256) or length code
;;;             (257..285)
;;;   dist[i] : distance code (0..29) when sym[i] > 256, else unused
;;;   el[i]   : match length (3..258) when sym[i] > 256, else unused
;;;   ed[i]   : match distance (1..32768) when sym[i] > 256, else unused

(defun run-lz77 (input start end level sym dist el ed lit-freq dist-freq)
  "Run LZ77 over INPUT[START,END), filling SYM/DIST/EL/ED (sized to the
input length) and the symbol frequency vectors.  Returns (VALUES NSYM
EXTRA-BITS) where EXTRA-BITS is the total number of length/distance extra
bits across all matches."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array input sym dist el ed lit-freq dist-freq)
           (type fixnum start end level))
  (let* ((head (make-array +hash-size+ :element-type 'fixnum :initial-element -1))
         (prev (make-array +window-size+ :element-type 'fixnum :initial-element -1))
         (nsym 0)
         (extra-bits 0)
         (max-chain (chain-limit level))
         (nice (nice-length level))
         (pos start))
    (declare (type fixnum nsym extra-bits max-chain nice pos))
    (loop while (< pos end) do
      (if (< (- end pos) +min-match+)
          (progn
            (let ((b (aref input pos)))
              (setf (aref sym nsym) b
                    (aref dist nsym) 0
                    (aref el nsym) 0
                    (aref ed nsym) 0)
              (incf (aref lit-freq b))
              (incf nsym))
            (incf pos))
          (progn
            (multiple-value-bind (len d)
                (longest-match input pos end head prev max-chain nice)
              (insert-string input pos head prev)
              (if (>= len +min-match+)
                  (let* ((code (length-code len))
                         (dcode (dist-code d)))
                    (declare (type fixnum code dcode))
                    (unless (<= d (- pos start))
                      (error "cl-newzlib internal: distance ~A exceeds emitted ~A at pos ~A"
                             d (- pos start) pos))
                    (setf (aref sym nsym) (+ 257 code)
                          (aref dist nsym) dcode
                          (aref el nsym) len
                          (aref ed nsym) d)
                    (incf (aref lit-freq (+ 257 code)))
                    (incf (aref dist-freq dcode))
                    (incf extra-bits (+ (length-extra-bits code)
                                        (dist-extra-bits dcode)))
                    (incf nsym)
                    (loop for q from (1+ pos) below (+ pos len)
                          do (when (< (+ q 2) end)
                               (insert-string input q head prev)))
                    (incf pos len))
                  (progn
                    (let ((b (aref input pos)))
                      (setf (aref sym nsym) b
                            (aref dist nsym) 0
                            (aref el nsym) 0
                            (aref ed nsym) 0)
                      (incf (aref lit-freq b))
                      (incf nsym))
                    (incf pos)))))))
    (setf (aref sym nsym) 256
          (aref dist nsym) 0
          (aref el nsym) 0
          (aref ed nsym) 0)
    (incf (aref lit-freq 256))
    (incf nsym)
    (values nsym extra-bits)))

;;; ------------------------------------------------------------------
;;; Block emission
;;; ------------------------------------------------------------------

(declaim (inline align-writer))
(defun align-writer (writer)
  "Pad WRITER with zero bits up to the next byte boundary."
  (declare (optimize (speed 3) (safety 0)))
  (let ((pad (logand 7 (bw-nbits writer))))
    (when (plusp pad)
      (write-bits writer 0 (- 8 pad)))))

(defun stored-block-bits (writer n)
  "Bit count of a stored block of N bytes given WRITER's current alignment."
  (let ((pad (mod (- 8 (+ (logand 7 (bw-nbits writer)) 3)) 8)))
    (+ 3 pad 32 (* 8 n))))

(defun emit-stored-block (writer input start end bfinal)
  (declare (optimize (speed 3) (safety 0))
           (type simple-array input)
           (type fixnum start end))
  (write-bits writer (if bfinal 1 0) 1)
  (write-bits writer 0 2)
  (align-writer writer)
  (let ((len (- end start)))
    (write-bits writer len 16)
    (write-bits writer (logand (lognot len) #xFFFF) 16))
  (loop for i from start below end do
    (write-bits writer (aref input i) 8)))

(defun emit-fixed-block (writer sym dist el ed nsym bfinal)
  (declare (optimize (speed 3) (safety 0))
           (type simple-array sym dist el ed)
           (type fixnum nsym))
  (ensure-static-trees)
  (write-bits writer (if bfinal 1 0) 1)
  (write-bits writer 1 2)
  (loop for i below nsym do
    (let ((s (aref sym i)))
      (write-bits writer (aref +static-lit-codes+ s)
                  (aref +static-lit-lengths+ s))
      (when (> s 256)
        (let* ((code (- s 257)))
          (declare (type fixnum code))
          (let ((n (length-extra-bits code)))
            (when (plusp n)
              (write-bits writer (- (aref el i) (length-base code)) n))))
        (let ((d (aref dist i)))
          (declare (type fixnum d))
          (write-bits writer (aref +static-dist-codes+ d) 5)
          (let ((n (dist-extra-bits d)))
            (when (plusp n)
              (write-bits writer (- (aref ed i) (dist-base d)) n))))))))

(defun data-bits (sym dist nsym len-array dist-len-array extra-bits)
  "Total coded bits for the token stream, plus the accumulated EXTRA-BITS."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array sym dist len-array dist-len-array)
           (type fixnum nsym extra-bits))
  (let ((bits extra-bits))
    (declare (type fixnum bits))
    (loop for i below nsym do
      (let ((s (aref sym i)))
        (incf bits (aref len-array s))
        (when (> s 256)
          (incf bits (aref dist-len-array (aref dist i))))))
    bits))

(defun scan-code-lengths (lengths n bl-sym bl-extra bl-freq nbl)
  "RLE-encode LENGTHS[0..N) into code-length symbols (RFC 1951 3.2.7),
appending to BL-SYM/BL-EXTRA starting at NBL and counting frequencies into
BL-FREQ.  Returns (VALUES NBL EXTRA-BITS)."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array lengths bl-sym bl-extra bl-freq)
           (type fixnum n nbl))
  (let ((count 0)
        (prevlen -1)
        (extra-bits 0))
    (declare (type fixnum count prevlen extra-bits))
    (let ((nextlen (if (plusp n) (aref lengths 0) -1))
          (max-count 7)
          (min-count 4))
      (declare (type fixnum nextlen max-count min-count))
      (when (zerop nextlen) (setf max-count 138 min-count 3))
      (loop for idx from 0 below n do
        (let ((curlen nextlen))
          (setf nextlen (if (< idx (1- n)) (aref lengths (1+ idx)) -1))
          (incf count)
          (unless (and (< count max-count) (= curlen nextlen))
            (cond
              ((< count min-count)
               (loop repeat count do
                 (setf (aref bl-sym nbl) curlen
                       (aref bl-extra nbl) 0)
                 (incf (aref bl-freq curlen))
                 (incf nbl)))
              ((not (zerop curlen))
               (when (/= curlen prevlen)
                 (setf (aref bl-sym nbl) curlen
                       (aref bl-extra nbl) 0)
                 (incf (aref bl-freq curlen))
                 (incf nbl)
                 (decf count))
               (setf (aref bl-sym nbl) 16
                     (aref bl-extra nbl) (- count 3))
               (incf (aref bl-freq 16))
               (incf extra-bits 2)
               (incf nbl))
              ((<= count 10)
               (setf (aref bl-sym nbl) 17
                     (aref bl-extra nbl) (- count 3))
               (incf (aref bl-freq 17))
               (incf extra-bits 3)
               (incf nbl))
              (t
               (setf (aref bl-sym nbl) 18
                     (aref bl-extra nbl) (- count 11))
               (incf (aref bl-freq 18))
               (incf extra-bits 7)
               (incf nbl)))
            (setf count 0 prevlen curlen)
            (cond ((zerop nextlen) (setf max-count 138 min-count 3))
                  ((= curlen nextlen) (setf max-count 6 min-count 3))
                  (t (setf max-count 7 min-count 4))))))
      (values nbl extra-bits))))

(defun emit-dynamic-block (writer sym dist el ed nsym bfinal
                            lit-codes lit-lengths dist-codes dist-lengths
                            bl-sym bl-extra bl-codes bl-lengths nbl
                            hlit hdist hclen)
  (declare (optimize (speed 3) (safety 0))
           (type simple-array sym dist el ed lit-codes lit-lengths
                              dist-codes dist-lengths bl-sym bl-extra
                              bl-codes bl-lengths)
           (type fixnum nsym nbl hlit hdist hclen))
  (write-bits writer (if bfinal 1 0) 1)
  (write-bits writer 2 2)
  (write-bits writer (- hlit 257) 5)
  (write-bits writer (- hdist 1) 5)
  (write-bits writer (- hclen 4) 4)
  (loop for i below hclen do
    (write-bits writer (aref bl-lengths (aref +code-length-order+ i)) 3))
  (loop for i below nbl do
    (let ((s (aref bl-sym i)))
      (write-bits writer (aref bl-codes s) (aref bl-lengths s))
      (case s
        (16 (write-bits writer (aref bl-extra i) 2))
        (17 (write-bits writer (aref bl-extra i) 3))
        (18 (write-bits writer (aref bl-extra i) 7))
        (otherwise nil))))
  (loop for i below nsym do
    (let ((s (aref sym i)))
      (write-bits writer (aref lit-codes s) (aref lit-lengths s))
      (when (> s 256)
        (let* ((code (- s 257)))
          (declare (type fixnum code))
          (let ((n (length-extra-bits code)))
            (when (plusp n)
              (write-bits writer (- (aref el i) (length-base code)) n))))
        (let ((d (aref dist i)))
          (declare (type fixnum d))
          (write-bits writer (aref dist-codes d) (aref dist-lengths d))
          (let ((n (dist-extra-bits d)))
            (when (plusp n)
              (write-bits writer (- (aref ed i) (dist-base d)) n))))))))

;;; ------------------------------------------------------------------
;;; Compressor driver
;;; ------------------------------------------------------------------

(defun deflate-into-writer (input start end writer level)
  "Compress INPUT[START,END) into WRITER as one DEFLATE stream.  Level 0
emits stored blocks; higher levels pick the cheapest block encoding."
  (declare (type simple-array input)
           (type fixnum start end level))
  (let ((n (- end start)))
    (if (zerop level)
        (if (< start end)
            (loop for s from start below end by +max-stored-block+
                  do (let ((e (min end (+ s +max-stored-block+))))
                       (emit-stored-block writer input s e (>= e end))))
            (emit-stored-block writer input start end t))
        (let ((sym (make-array (1+ n) :element-type 'fixnum))
              (dist (make-array (1+ n) :element-type 'fixnum))
              (el (make-array (1+ n) :element-type 'fixnum))
              (ed (make-array (1+ n) :element-type 'fixnum))
              (lit-freq (make-array +heap-size+ :element-type 'fixnum
                                    :initial-element 0))
              (dist-freq (make-array +heap-size+ :element-type 'fixnum
                                     :initial-element 0)))
          (multiple-value-bind (nsym extra-bits)
              (run-lz77 input start end level sym dist el ed lit-freq dist-freq)
            (multiple-value-bind (lit-lengths lit-codes lit-max)
                (build-huffman-codes lit-freq 286 15)
              (declare (ignore lit-max))
              (multiple-value-bind (dist-lengths dist-codes dist-max)
                  (build-huffman-codes dist-freq 30 15)
                (declare (ignore dist-max))
                (let ((hlit (max 257 (1+ (loop for i from 285 downto 256
                                              when (plusp (aref lit-lengths i))
                                              return i))))
                      (hdist (max 1 (1+ (loop for i from 29 downto 0
                                             when (plusp (aref dist-lengths i))
                                             return i)))))
                  (let ((bl-sym (make-array 320 :element-type 'fixnum))
                        (bl-extra (make-array 320 :element-type 'fixnum))
                        (bl-freq (make-array +heap-size+ :element-type 'fixnum
                                             :initial-element 0)))
                    (multiple-value-bind (nbl1 bl-extra1)
                        (scan-code-lengths lit-lengths hlit bl-sym bl-extra bl-freq 0)
                      (declare (ignore bl-extra1))
                      (multiple-value-bind (nbl2 bl-extra2)
                          (scan-code-lengths dist-lengths hdist bl-sym bl-extra bl-freq nbl1)
                        (multiple-value-bind (bl-lengths bl-codes bl-max)
                            (build-huffman-codes bl-freq 19 7)
                          (declare (ignore bl-max))
                          (let ((hclen 4))
                            (declare (type fixnum hclen))
                            (loop for rank from 18 downto 3
                                  when (plusp (aref bl-lengths
                                                   (aref +code-length-order+ rank)))
                                  do (setf hclen (1+ rank))
                                  (return))
                            (let ((bl-code-bits 0))
                              (declare (type fixnum bl-code-bits))
                              (loop for i below nbl2 do
                                (incf bl-code-bits
                                      (aref bl-lengths (aref bl-sym i))))
                              (ensure-static-trees)
                              (let ((opt-size (+ 3 14 (* 3 hclen) bl-extra2 bl-code-bits
                                                 (data-bits sym dist nsym lit-lengths
                                                            dist-lengths extra-bits)))
                                    (fixed-size (+ 3 (data-bits sym dist nsym
                                                               +static-lit-lengths+
                                                               +static-dist-lengths+
                                                               extra-bits)))
                                    (stored-size (stored-block-bits writer n)))
                                (cond
                                  ((<= stored-size (min opt-size fixed-size))
                                   (emit-stored-block writer input start end t))
                                  ((<= fixed-size opt-size)
                                   (emit-fixed-block writer sym dist el ed nsym t))
                                  (t
                                   (emit-dynamic-block writer sym dist el ed nsym t
                                                       lit-codes lit-lengths
                                                       dist-codes dist-lengths
                                                       bl-sym bl-extra bl-codes bl-lengths
                                                       nbl2 hlit hdist hclen))))))))))))))))))

(defun deflate-raw (input &optional (start 0) (end (length input)) (level 6))
  "Compress INPUT[START,END) with raw DEFLATE (no zlib/gzip header) at LEVEL.
Returns a fresh octet vector."
  (check-compression-level level)
  (unless (typep input '(simple-array (unsigned-byte 8) (*)))
    (error 'newzlib-parameter-error :detail "input must be an (unsigned-byte 8) vector"))
  (let ((writer (make-bit-writer 1024)))
    (deflate-into-writer input start end writer level)
    (writer-bytes writer)))
