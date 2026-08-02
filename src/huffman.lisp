(in-package #:cl-newzlib)

;;; DEFLATE Huffman coding (RFC 1951).
;;;
;;; Two directions live here:
;;;   * Decode tables built from code lengths (for inflate), and
;;;   * Length-limited code construction from symbol frequencies (for the
;;;     compressor), ported faithfully from zlib's build_tree/gen_bitlen/
;;;     gen_codes (trees.c) so that the emitted trees are exactly valid
;;;     DEFLATE trees (max 15 bits for lit/dist, 7 bits for code lengths).

(defconstant +literals+ 256)
(defconstant +l-codes+ 286)
(defconstant +d-codes+ 30)
(defconstant +bl-codes+ 19)
(defconstant +max-code-length+ 15)
(defconstant +max-bl-bits+ 7)
(defconstant +end-block+ 256)

(defparameter +length-extra-bits+
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter +length-base+
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter +dist-extra-bits+
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
(defparameter +dist-base+
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
       1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter +code-length-order+
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

;;; Map match lengths (3..258) to length codes (0..28), and distances
;;; (1..32768) to distance codes (0..29), using zlib's construction.
(defun compute-length-code-table ()
  (let ((table (make-array 256 :element-type '(unsigned-byte 8)))
        (length 0)
        (code 0))
    (declare (type fixnum length code))
    (loop while (< code 28) do
      (let ((n (ash 1 (aref +length-extra-bits+ code))))
        (declare (type fixnum n))
        (loop repeat n do
          (setf (aref table length) code
                length (1+ length)))
        (incf code)))
    (setf (aref table 255) 28)
    table))

(defun compute-dist-code-table ()
  (let ((table (make-array 512 :element-type '(unsigned-byte 8)))
        (dist 0)
        (code 0))
    (declare (type fixnum dist code))
    (loop while (< code 16) do
      (let ((n (ash 1 (aref +dist-extra-bits+ code))))
        (declare (type fixnum n))
        (loop repeat n do
          (setf (aref table dist) code
                dist (1+ dist)))
        (incf code)))
    (setf dist (ash dist -7))
    (loop while (< code 30) do
      (let ((n (ash 1 (- (aref +dist-extra-bits+ code) 7))))
        (declare (type fixnum n))
        (loop repeat n do
          (setf (aref table (+ 256 dist)) code
                dist (1+ dist)))
        (incf code)))
    table))

(defparameter +length-code+ (compute-length-code-table))
(defparameter +dist-code+ (compute-dist-code-table))

(declaim (inline length-code dist-code length-extra-bits dist-extra-bits
                 length-base dist-base))
(defun length-code (match-length)
  (aref +length-code+ (- match-length 3)))
(defun dist-code (distance)
  "Return the DEFLATE distance code (0..29) for DISTANCE (1..32768)."
  (if (< distance 257)
      (aref +dist-code+ (1- distance))
      (aref +dist-code+ (+ 256 (ash (1- distance) -7)))))
(defun length-extra-bits (code) (aref +length-extra-bits+ code))
(defun dist-extra-bits (code) (aref +dist-extra-bits+ code))
(defun length-base (code) (aref +length-base+ code))
(defun dist-base (code) (aref +dist-base+ code))

;;; ------------------------------------------------------------------
;;; Decode tables (for inflate)
;;; ------------------------------------------------------------------

(defstruct (huffman-decode-table
            (:conc-name hdt-)
            (:constructor make-hdt (counts first offsets symbols max-length
                                          root fast index-root)))
  ;; counts[l]    : number of symbols with code length l
  ;; first[l]     : canonical first code value of length l (zlib's first[])
  ;; offsets[l]   : index into symbols where length-l symbols start
  ;; symbols      : symbols ordered by (length, canonical code)
  ;; max-length   : longest code length in the table
  ;; root         : bits looked up at once in FAST (table size 2^ROOT)
  ;; fast         : 2^ROOT table; entry 0 = slow path (code longer than ROOT),
  ;;                else (LENGTH << 9) | SYMBOL for a code of LENGTH <= ROOT
  ;; index-root   : number of symbols with code length <= ROOT
  (counts nil :read-only t)
  (first nil :read-only t)
  (offsets nil :read-only t)
  (symbols nil :read-only t)
  (max-length 0 :read-only t :type fixnum)
  (root 0 :read-only t :type fixnum)
  (fast nil :read-only t)
  (index-root 0 :read-only t :type fixnum))

(defun build-huffman-decode-table (lengths &optional (start 0) (n (length lengths))
                                             (root 9))
  "Build a canonical Huffman decode table from the code lengths in
LENGTHS[START,START+N).  ROOT bits are looked up at once through a fast
jump table; codes longer than ROOT fall back to a canonical walk.  Returns
a HUFFMAN-DECODE-TABLE."
  (declare (type simple-array lengths)
           (type fixnum start n root)
           (optimize (speed 3) (safety 0)))
  (let ((counts (make-array (1+ +max-code-length+) :element-type 'fixnum
                            :initial-element 0))
        (first (make-array (1+ +max-code-length+) :element-type 'fixnum
                           :initial-element 0))
        (offsets (make-array (1+ +max-code-length+) :element-type 'fixnum
                             :initial-element 0)))
    (declare (type simple-array counts first offsets))
    (loop for i from start below (+ start n) do
      (let ((l (aref lengths i)))
        (declare (type fixnum l))
        (when (plusp l)
          (when (> l +max-code-length+)
            (error 'newzlib-format-error :detail "code length exceeds 15"))
          (incf (aref counts l)))))
    ;; verify the lengths satisfy Kraft's inequality
    (let ((kraft 0))
      (loop for l from 1 to +max-code-length+ do
        (setf kraft (+ kraft (ash (aref counts l) (- +max-code-length+ l))))
        (setf (aref offsets l) (+ (aref offsets (1- l)) (aref counts (1- l))))
        (setf (aref first l) (ash (+ (aref first (1- l)) (aref counts (1- l))) 1)))
      (when (> kraft (ash 1 +max-code-length+))
        (error 'newzlib-format-error :detail "invalid Huffman code lengths")))
    (let ((symbols (make-array n :element-type 'fixnum :initial-element 0))
          (max-length 0))
      (declare (type simple-array symbols))
      (loop for l from 1 to +max-code-length+
            for k fixnum = (aref offsets l) then k do
        (when (plusp (aref counts l))
          (loop for i from start below (+ start n) do
            (when (= (aref lengths i) l)
              (setf (aref symbols k) (- i start))
              (incf k)))))
      (loop for l from +max-code-length+ downto 1
            when (plusp (aref counts l)) do (setf max-length l) (return))
      (let ((root (min (max 1 root) max-length)))
        (declare (type fixnum root))
        (let* ((size (ash 1 root))
               (fast (make-array size :element-type 'fixnum :initial-element 0))
               (index-root 0))
          (declare (type fixnum size index-root))
          (loop for l from 1 to root do (incf index-root (aref counts l)))
          ;; for each code length L <= ROOT, fill the indices whose low L
          ;; bits equal the bit-reversed (LSB-first) code of every symbol of
          ;; length L, so that the table is keyed by the reader's hold value
          (loop for l from 1 to root
                when (plusp (aref counts l)) do
            (loop for k from (aref offsets l) below (+ (aref offsets l)
                                                       (aref counts l)) do
              (let* ((sym (aref symbols k))
                     (entry (logior (ash l 9) sym))
                     (c (+ (aref first l) (- k (aref offsets l))))
                     (idx (reverse-bits c l)))
                (declare (type fixnum sym entry c idx))
                (dotimes (i (ash 1 (- root l)))
                  (setf (aref fast idx) entry)
                  (incf idx (ash 1 l))))))
          (make-hdt counts first offsets symbols max-length root fast index-root))))))

(declaim (inline huffman-decode))
(defun huffman-decode (table reader)
  "Decode the next Huffman symbol from READER using TABLE.  Looks up the
first ROOT bits through a jump table; only codes longer than ROOT fall
back to reading bits one at a time."
  (declare (type huffman-decode-table table)
           (optimize (speed 3) (safety 0)))
  (let* ((root (hdt-root table))
         (v (peek-bits-capped reader root))
         (entry (aref (hdt-fast table) v)))
    (declare (type fixnum root v entry))
    (if (zerop entry)
        ;; slow path: code longer than ROOT bits; consume the ROOT bits we
        ;; peeked, then walk the remaining bits, accumulating the canonical
        ;; code MSB-first (bit-reversing the ROOT bits we already hold)
        (let ((counts (hdt-counts table))
              (first (hdt-first table))
              (symbols (hdt-symbols table))
              (code (reverse-bits v root))
              (index (hdt-index-root table))
              (len root))
          (declare (type fixnum code index len))
          (read-bits reader root)
          (block decode
            (loop do
              (incf len)
              (setf code (logior (ash code 1) (read-bits reader 1)))
              (let ((count (aref counts len)))
                (declare (type fixnum count))
                (when (< (- code count) (aref first len))
                  (return-from decode
                    (aref symbols (+ index (- code (aref first len))))))
                (setf index (+ index count))))
            (error 'newzlib-format-error :detail "invalid Huffman code")))
        ;; fast path
        (progn
          (read-bits reader (ash entry -9))
          (logand entry #x1FF)))))

;;; ------------------------------------------------------------------
;;; Static tree tables (fixed blocks)
;;; ------------------------------------------------------------------

(defun compute-static-lit-tree ()
  "Return (VALUES CODES LENGTHS) for the RFC 1951 fixed literal/length
tree over symbols 0..287.  CODES holds bit-reversed canonical codes so they
can be emitted directly with the LSB-first writer."
  (let* ((lengths (make-array 288 :element-type 'fixnum))
         (codes (make-array 288 :element-type 'fixnum))
         (bl-count (make-array 16 :element-type 'fixnum :initial-element 0)))
    (loop for n from 0 below 144 do (setf (aref lengths n) 8))
    (loop for n from 144 below 256 do (setf (aref lengths n) 9))
    (loop for n from 256 below 280 do (setf (aref lengths n) 7))
    (loop for n from 280 below 288 do (setf (aref lengths n) 8))
    (loop for n from 0 below 288 do (incf (aref bl-count (aref lengths n))))
    (let ((next (make-array 16 :element-type 'fixnum :initial-element 0))
          (code 0))
      (declare (type fixnum code))
      (loop for bits from 1 to 15 do
        (setf code (ash (+ code (aref bl-count (1- bits))) 1))
        (setf (aref next bits) code))
      (loop for n from 0 below 288 do
        (let ((len (aref lengths n)))
          (when (plusp len)
            (setf (aref codes n) (reverse-bits (aref next len) len))
            (incf (aref next len))))))
    (values codes lengths)))

(defun reverse-bits (code len)
  (let ((res 0))
    (dotimes (i len res)
      (setf res (logior (ash res 1) (logand code 1))
            code (ash code -1)))))

(defparameter +static-lit-codes+ nil)
(defparameter +static-lit-lengths+ nil)
(defparameter +static-dist-codes+ nil)
(defparameter +static-dist-lengths+ nil)

(defun ensure-static-trees ()
  (unless +static-lit-codes+
    (multiple-value-bind (codes lengths)
        (compute-static-lit-tree)
      (setf +static-lit-codes+ codes
            +static-lit-lengths+ lengths))
    (let ((codes (make-array 30 :element-type 'fixnum))
          (lengths (make-array 30 :element-type 'fixnum :initial-element 5)))
      (dotimes (n 30)
        (setf (aref codes n) (reverse-bits n 5)))
      (setf +static-dist-codes+ codes
            +static-dist-lengths+ lengths)))
  (values +static-lit-codes+ +static-lit-lengths+
          +static-dist-codes+ +static-dist-lengths+))

;;; ------------------------------------------------------------------
;;; Length-limited code construction (for the compressor)
;;; ------------------------------------------------------------------
;;;
;;; Faithful port of zlib trees.c build_tree + gen_bitlen + gen_codes.
;;; It builds the optimal Huffman tree with a heap, computes the code
;;; lengths, and if any exceed MAX-LENGTH applies zlib's bl_count
;;; adjustment so the result is a valid length-limited canonical code.

(defconstant +heap-size+ (1+ (* 2 +l-codes+)))

(defun build-huffman-codes (freq elems max-length)
  "Given FREQ (a vector of symbol frequencies, length >= +HEAP-SIZE+, entries
>= ELEMS used as scratch for internal nodes), compute a length-limited
canonical Huffman code.  Returns (VALUES LENGTHS CODES MAX-CODE) where
LENGTHS[i] is the code length (0 for unused), CODES[i] is the bit-reversed
canonical code value (0 for unused), and MAX-CODE is the largest symbol
index with a non-zero frequency."
  (declare (type simple-array freq)
           (type fixnum elems max-length)
           (optimize (speed 3) (safety 0)))
  (let* ((heap (make-array +heap-size+ :element-type 'fixnum))
         (dad (make-array +heap-size+ :element-type 'fixnum))
         (depth (make-array +heap-size+ :element-type 'fixnum))
         (node-length (make-array +heap-size+ :element-type 'fixnum))
         (leaf-lengths (make-array elems :element-type 'fixnum))
         (codes (make-array elems :element-type 'fixnum))
         (bl-count (make-array (1+ +max-code-length+) :element-type 'fixnum
                               :initial-element 0))
         (heap-len 0)
         (heap-max +heap-size+)
         (max-code -1)
         (overflow 0))
    (declare (type fixnum heap-len heap-max max-code overflow))
    (labels ((smaller (n m)
               (let ((fn (aref freq n))
                     (fm (aref freq m)))
                 (or (< fn fm)
                     (and (= fn fm) (<= (aref depth n) (aref depth m))))))
             (pqdownheap (k)
               (let ((v (aref heap k)))
                 (declare (type fixnum v))
                 (loop for j = (ash k 1) then (ash j 1) do
                   (when (and (< j heap-len)
                              (smaller (aref heap (1+ j)) (aref heap j)))
                     (incf j))
                   (when (or (> j heap-len) (smaller v (aref heap j)))
                     (return))
                   (setf (aref heap k) (aref heap j)
                         k j))
                 (setf (aref heap k) v)))
             (pqremove ()
               (let ((top (aref heap 1)))
                 (setf (aref heap 1) (aref heap heap-len)
                       heap-len (1- heap-len))
                 (pqdownheap 1)
                 top)))
      ;; construct the initial heap of leaves with non-zero frequency
      (loop for n from 0 below elems do
        (if (plusp (aref freq n))
            (progn
              (setf heap-len (1+ heap-len)
                    (aref heap heap-len) n
                    max-code n
                    (aref depth n) 0))
            (setf (aref leaf-lengths n) 0)))
      ;; force at least two codes of non-zero frequency (pkzip requirement)
      (loop while (< heap-len 2) do
        (let ((node (if (< max-code 2) (incf max-code) 0)))
          (setf heap-len (1+ heap-len)
                (aref heap heap-len) node
                (aref freq node) 1
                (aref depth node) 0)))
      ;; heapify
      (loop for n from (floor heap-len 2) downto 1 do (pqdownheap n))
      ;; combine least-frequent nodes into internal nodes
      (let ((node elems))
        (declare (type fixnum node))
        (loop while (>= heap-len 2) do
          (let ((n (pqremove))
                (m (aref heap 1)))
            (declare (type fixnum n m))
            (setf heap-max (1- heap-max)
                  (aref heap heap-max) n)
            (setf heap-max (1- heap-max)
                  (aref heap heap-max) m)
            (setf (aref freq node) (+ (aref freq n) (aref freq m))
                  (aref depth node) (1+ (max (aref depth n) (aref depth m)))
                  (aref dad n) node
                  (aref dad m) node
                  (aref heap 1) node)
            (pqdownheap 1)
            (incf node)))
        (setf heap-max (1- heap-max)
              (aref heap heap-max) (aref heap 1)))
      ;; gen_bitlen: compute lengths from the tree
      (setf (aref node-length (aref heap heap-max)) 0)
      (loop for h from (1+ heap-max) below +heap-size+ do
        (let* ((n (aref heap h))
               (bits (1+ (aref node-length (aref dad n)))))
          (declare (type fixnum n bits))
          (when (> bits max-length)
            (setf bits max-length)
            (incf overflow))
          (setf (aref node-length n) bits)
          (when (<= n max-code)
            (incf (aref bl-count bits)))))
      (when (plusp overflow)
        ;; adjust bl-count to force the code into max-length bits
        (loop while (> overflow 0) do
          (let ((bits (1- max-length)))
            (loop while (zerop (aref bl-count bits)) do (decf bits))
            (decf (aref bl-count bits))
            (incf (aref bl-count (1+ bits)) 2)
            (decf (aref bl-count max-length))
            (decf overflow 2)))
        ;; recompute the lengths, scanning the heap in increasing frequency
        (let ((h +heap-size+))
          (declare (type fixnum h))
          (loop for bits from max-length downto 1 do
            (let ((n (aref bl-count bits)))
              (loop while (plusp n) do
                (decf h)
                (let ((m (aref heap h)))
                  (when (<= m max-code)
                    (setf (aref node-length m) bits)
                    (decf n))))))))
      ;; copy leaf lengths out
      (loop for n from 0 below elems do
        (setf (aref leaf-lengths n) (aref node-length n)))
      ;; gen_codes: assign canonical codes, bit-reversed for LSB-first output
      (let ((next (make-array (1+ +max-code-length+) :element-type 'fixnum
                              :initial-element 0))
            (code 0))
        (declare (type fixnum code))
        (loop for bits from 1 to +max-code-length+ do
          (setf code (ash (+ code (aref bl-count (1- bits))) 1))
          (setf (aref next bits) code))
        (loop for n from 0 to max-code do
          (let ((len (aref leaf-lengths n)))
            (when (plusp len)
              (setf (aref codes n) (reverse-bits (aref next len) len))
              (incf (aref next len))))))
      (values leaf-lengths codes max-code))))
