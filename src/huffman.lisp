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
(defconstant +max-decode-root+ 12)
(defconstant +max-bl-bits+ 7)
(defconstant +end-block+ 256)

(defparameter +length-extra-bits+
  (make-array 29 :element-type '(unsigned-byte 8)
              :initial-contents '(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3
                                  4 4 4 4 5 5 5 5 0)))
(defparameter +length-base+
  (make-array 29 :element-type '(unsigned-byte 16)
              :initial-contents '(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31
                                  35 43 51 59 67 83 99 115 131 163 195 227 258)))
(defparameter +dist-extra-bits+
  (make-array 30 :element-type '(unsigned-byte 8)
              :initial-contents '(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6
                                  7 7 8 8 9 9 10 10 11 11 12 12 13 13)))
(defparameter +dist-base+
  (make-array 30 :element-type '(unsigned-byte 16)
              :initial-contents '(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193
                                  257 385 513 769 1025 1537 2049 3073 4097
                                  6145 8193 12289 16385 24577)))
(defparameter +code-length-order+
  (make-array 19 :element-type '(unsigned-byte 8)
              :initial-contents '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)))

;;; The compiler needs these types to keep the per-match table lookups
;;; (LENGTH-CODE/DIST-CODE/extras/bases) off the generic array-access and
;;; boxed-arithmetic paths.
(declaim (type (simple-array (unsigned-byte 8) (*))
               +length-extra-bits+ +dist-extra-bits+ +code-length-order+
               +length-code+ +dist-code+)
         (type (simple-array (unsigned-byte 16) (*))
               +length-base+ +dist-base+))

;;; Map match lengths (3..258) to length codes (0..28), and distances
;;; (1..32768) to distance codes (0..29), using zlib's construction.
(defun compute-length-code-table ()
  (let ((table (make-array 256 :element-type '(unsigned-byte 8)))
        (length 0)
        (code 0))
    (declare (type fixnum length code))
    (iterate:iterate
      (iterate:while (< code 28))
      (let ((n (ash 1 (aref +length-extra-bits+ code))))
        (declare (type fixnum n))
        (iterate:iterate (iterate:repeat n)
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
    (iterate:iterate
      (iterate:while (< code 16))
      (let ((n (ash 1 (aref +dist-extra-bits+ code))))
        (declare (type fixnum n))
        (iterate:iterate (iterate:repeat n)
          (setf (aref table dist) code
                dist (1+ dist)))
        (incf code)))
    (setf dist (ash dist -7))
    (iterate:iterate
      (iterate:while (< code 30))
      (let ((n (ash 1 (- (aref +dist-extra-bits+ code) 7))))
        (declare (type fixnum n))
        (iterate:iterate (iterate:repeat n)
          (setf (aref table (+ 256 dist)) code
                dist (1+ dist)))
        (incf code)))
    table))

(defparameter +length-code+ (compute-length-code-table))
(defparameter +dist-code+ (compute-dist-code-table))

;;; Combined lookup tables for the hot match paths: one load yields the
;;; code plus its extra-bits count (by length/distance), or the base plus
;;; its extra-bits count (by code).  Hot loops do ~2M of these lookups per
;;; megabyte, so halving the table traffic matters.
;;;
;;; Layouts (all unsigned-32):
;;;   +length-code+extra+[len-3]: code in bits 0..7, extra in 8..15
;;;   +dist-code+extra+[dist-key]: dcode in 0..7, extra in 8..15, with the
;;;     same keying as +dist-code+ (d-1 below 257, 256+((d-1)>>7) above)
;;;   +length-base+extra+[code]: base in 0..15, extra in 16..23
;;;   +dist-base+extra+[dcode]: base in 0..15, extra in 16..23

(defun compute-length-code+extra-table ()
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (i 256 table)
      (let ((code (aref +length-code+ i)))
        (setf (aref table i)
              (logior code (ash (aref +length-extra-bits+ code) 8)))))))

(defun compute-dist-code+extra-table ()
  (let ((table (make-array 512 :element-type '(unsigned-byte 32))))
    (dotimes (i 512 table)
      (let ((code (aref +dist-code+ i)))
        (setf (aref table i)
              (logior code (ash (aref +dist-extra-bits+ code) 8)))))))

(defun compute-length-base+extra-table ()
  (let ((table (make-array 29 :element-type '(unsigned-byte 32))))
    (dotimes (code 29 table)
      (setf (aref table code)
            (logior (aref +length-base+ code)
                    (ash (aref +length-extra-bits+ code) 16))))))

(defun compute-dist-base+extra-table ()
  (let ((table (make-array 30 :element-type '(unsigned-byte 32))))
    (dotimes (code 30 table)
      (setf (aref table code)
            (logior (aref +dist-base+ code)
                    (ash (aref +dist-extra-bits+ code) 16))))))

(defparameter +length-code+extra+ (compute-length-code+extra-table))
(defparameter +dist-code+extra+ (compute-dist-code+extra-table))
(defparameter +length-base+extra+ (compute-length-base+extra-table))
(defparameter +dist-base+extra+ (compute-dist-base+extra-table))

(declaim (type (simple-array (unsigned-byte 32) (*))
               +length-code+extra+ +dist-code+extra+
               +length-base+extra+ +dist-base+extra+))

(declaim (inline length-code dist-code length-extra-bits dist-extra-bits
                 length-base dist-base
                 length-code+extra dist-code+extra
                 length-base+extra dist-base+extra))
(defun length-code (match-length)
  (aref +length-code+ (- match-length 3)))
(defun dist-code (distance)
  "Return the DEFLATE distance code (0..29) for DISTANCE (1..32768)."
  (if (< distance 257)
      (aref +dist-code+ (1- distance))
      (aref +dist-code+ (+ 256 (ash (1- distance) -7)))))
(defun length-code+extra (match-length)
  "Length code and extra-bits count for MATCH-LENGTH (3..258) packed as
CODE | EXTRA<<8."
  (aref +length-code+extra+ (- match-length 3)))
(defun dist-code+extra (distance)
  "Distance code and extra-bits count for DISTANCE, packed as
DCODE | EXTRA<<8 (same keying as DIST-CODE)."
  (if (< distance 257)
      (aref +dist-code+extra+ (1- distance))
      (aref +dist-code+extra+ (+ 256 (ash (1- distance) -7)))))
(defun length-base+extra (code)
  "Length base and extra-bits count for a length CODE, packed as
BASE | EXTRA<<16."
  (aref +length-base+extra+ code))
(defun dist-base+extra (dcode)
  "Distance base and extra-bits count for a distance code, packed as
BASE | EXTRA<<16."
  (aref +dist-base+extra+ dcode))
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
  (counts nil :read-only t :type (simple-array (unsigned-byte 16) (*)))
  (first nil :read-only t :type (simple-array (unsigned-byte 16) (*)))
  (offsets nil :read-only t :type (simple-array (unsigned-byte 16) (*)))
  (symbols nil :read-only t :type (simple-array (unsigned-byte 16) (*)))
  (max-length 0 :read-only t :type fixnum)
  (root 0 :read-only t :type fixnum)
  (fast nil :read-only t :type (simple-array (unsigned-byte 16) (*)))
  (index-root 0 :read-only t :type fixnum))

;;; Huffman decode runs once per output symbol; keep its table accessors
;;; open-coded in the inflate loop.
(declaim (inline hdt-counts hdt-first hdt-offsets hdt-symbols hdt-max-length
                 hdt-root hdt-fast hdt-index-root))

;;; Bit reversal for canonical codes.  Defined before the table builder so
;;; its calls there open-code (it is also used by the static-tree code
;;; below).  The loop is tiny; the builder calls it once per short symbol.
(defun reverse-bits (code len)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum code len))
  (let ((res 0))
    (declare (type fixnum res))
    (dotimes (i len res)
      (setf res (logior (ash res 1) (logand code 1))
            code (ash code -1)))))

(declaim (inline reverse-bits))

(declaim (inline %count-code-lengths %init-decode-bases %place-decode-symbols
                 %fill-decode-fast))

(defun %count-code-lengths (lengths start n counts)
  "Count LENGTHS[START,START+N) into COUNTS, rejecting lengths above 15."
  (declare (type (simple-array fixnum (*)) lengths)
           (type (simple-array (unsigned-byte 16) (*)) counts)
           (type fixnum start n)
           (optimize (speed 3) (safety 0)))
  (loop for i from start below (+ start n) do
    (let ((l (aref lengths i)))
      (declare (type fixnum l))
      (when (plusp l)
        (when (> l +max-code-length+)
          (error 'newzlib-format-error :detail "code length exceeds 15"))
        (incf (aref counts l))))))

(defun %init-decode-bases (counts first offsets)
  "Verify Kraft's inequality; fill per-length OFFSETS and FIRST codes."
  (declare (type (simple-array (unsigned-byte 16) (*)) counts first offsets)
           (optimize (speed 3) (safety 0)))
  ;; The shift count is statically 0..14 (L ranges 1..15), but that needs
  ;; asserting for the compiler to emit a single-direction shift.
  (let ((kraft 0))
    (declare (type fixnum kraft))
    (loop for l from 1 to +max-code-length+ do
      (setf kraft (+ kraft (ash (aref counts l)
                                (the (integer 0 14)
                                     (- +max-code-length+ l)))))
      (setf (aref offsets l) (+ (aref offsets (1- l)) (aref counts (1- l))))
      (setf (aref first l) (ash (+ (aref first (1- l)) (aref counts (1- l))) 1)))
    (when (> kraft (ash 1 +max-code-length+))
      (error 'newzlib-format-error :detail "invalid Huffman code lengths"))))

(defun %place-decode-symbols (lengths start n offsets symbols counts)
  "Single-pass counting placement ordered by (length, index); seeds CURSORS
from OFFSETS.  Returns the longest used code length."
  (declare (type (simple-array fixnum (*)) lengths)
           (type (simple-array (unsigned-byte 16) (*)) offsets symbols counts)
           (type fixnum start n)
           (optimize (speed 3) (safety 0)))
  (let ((cursors (make-array (1+ +max-code-length+) :element-type 'fixnum
                             :initial-element 0))
        (top 0))
    (declare (type (simple-array fixnum (*)) cursors) (type fixnum top))
    (loop for l from 1 to +max-code-length+ do
      (setf (aref cursors l) (aref offsets l)))
    (loop for i from start below (+ start n) do
      (let ((l (aref lengths i)))
        (declare (type fixnum l))
        (when (plusp l)
          (setf (aref symbols (aref cursors l)) (- i start))
          (incf (aref cursors l)))))
    (loop for l from +max-code-length+ downto 1
          when (plusp (aref counts l)) do (setf top l) (return))
    top))

(defun %fill-decode-fast (counts first offsets symbols root)
  "Fill the 2^ROOT jump table keyed by LSB-first code; returns (VALUES
FAST INDEX-ROOT), the table and the count of symbols it covers."
  (declare (type (simple-array (unsigned-byte 16) (*))
                 counts first offsets symbols)
           (type fixnum root)
           (optimize (speed 3) (safety 0)))
  (let* ((size (ash 1 root))
         (fast (make-array size :element-type '(unsigned-byte 16)
                           :initial-element 0))
         (index-root 0))
    (declare (type fixnum size index-root)
             (type (simple-array (unsigned-byte 16) (*)) fast))
    (loop for l from 1 to root do (incf index-root (aref counts l)))
    ;; for each length L <= ROOT, fill indices whose low L bits equal the
    ;; bit-reversed code of every length-L symbol, keyed by the hold value
    (loop for l from 1 to root
          when (plusp (aref counts l)) do
      (loop for k from (aref offsets l) below (+ (aref offsets l)
                                                 (aref counts l)) do
        (let* ((sym (aref symbols k))
               (entry (logior (ash l 9) sym))
               (c (+ (aref first l) (- k (aref offsets l))))
               (idx (reverse-bits c l))
               (rep (ash 1 (the (integer 0 14) (- root l)))))
          (declare (type fixnum sym entry c idx rep))
          (dotimes (i rep)
            (setf (aref fast idx) entry)
            (incf idx (ash 1 l))))))
    (values fast index-root)))

(defun build-huffman-decode-table (lengths &optional (start 0) (n (length lengths))
                                             (root 10))
  "Build a canonical Huffman decode table from the code lengths in
LENGTHS[START,START+N).  ROOT bits look up at once through a fast jump
table; longer codes fall back to a canonical walk.  NIL selects an
adaptive root capped at +MAX-DECODE-ROOT+."
  (declare (type (simple-array fixnum (*)) lengths)
           (type fixnum start n)
           (type (or null fixnum) root)
           (optimize (speed 3) (safety 0)))
  (let ((counts (make-array (1+ +max-code-length+)
                            :element-type '(unsigned-byte 16)
                            :initial-element 0))
        (first (make-array (1+ +max-code-length+)
                           :element-type '(unsigned-byte 16)
                           :initial-element 0))
        (offsets (make-array (1+ +max-code-length+)
                             :element-type '(unsigned-byte 16)
                             :initial-element 0))
        (symbols (make-array n :element-type '(unsigned-byte 16)
                             :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 16) (*))
                   counts first offsets symbols))
    (%count-code-lengths lengths start n counts)
    (%init-decode-bases counts first offsets)
    (let ((max-length (%place-decode-symbols lengths start n offsets
                                              symbols counts)))
      (declare (type fixnum max-length))
      (let ((decode-root (if root
                             (min (max 1 root) max-length)
                             (min +max-decode-root+ (max 1 max-length)))))
        (declare (type fixnum decode-root))
        (multiple-value-bind (fast index-root)
            (%fill-decode-fast counts first offsets symbols decode-root)
          (make-hdt counts first offsets symbols max-length
                    decode-root fast index-root))))))

(declaim (inline huffman-decode))
(defun huffman-decode (table reader)
  "Decode the next Huffman symbol from READER using TABLE.  Looks up the
first ROOT bits through a jump table; only codes longer than ROOT fall
back to reading bits one at a time."
  (declare (type huffman-decode-table table)
           (optimize (speed 3) (safety 0)))
  (let* ((root (hdt-root table))
         (fast (hdt-fast table))
         (v (peek-bits-capped reader root))
         (entry (aref fast v)))
    (declare (type fixnum root v entry)
              (type (simple-array (unsigned-byte 16) (*)) fast))
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
          (declare (type fixnum code index len)
                    (type (simple-array (unsigned-byte 16) (*))
                           counts first symbols))
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

(declaim (inline %fill-canonical-codes))

(defun %fill-canonical-codes (lengths codes end bl-count next)
  "Assign bit-reversed canonical codes for symbols below END from BL-COUNT.
Shared by the static tree and the length-limited builder (gen_codes)."
  (declare (type (simple-array fixnum (*)) lengths codes bl-count next)
           (type fixnum end)
           (optimize (speed 3) (safety 0)))
  (let ((code 0))
    (declare (type fixnum code))
    (loop for bits from 1 to +max-code-length+ do
      (setf code (ash (+ code (aref bl-count (1- bits))) 1))
      (setf (aref next bits) code))
    (loop for n from 0 below end do
      (let ((len (aref lengths n)))
        (declare (type fixnum len))
        (when (plusp len)
          (setf (aref codes n) (reverse-bits (aref next len) len))
          (incf (aref next len)))))))

(defun compute-static-lit-tree ()
  "Return (VALUES CODES LENGTHS) for the RFC 1951 fixed literal/length
tree over symbols 0..287.  CODES holds bit-reversed canonical codes so they
can be emitted directly with the LSB-first writer."
  (let* ((lengths (make-array 288 :element-type 'fixnum))
         (codes (make-array 288 :element-type 'fixnum))
         (bl-count (make-array 16 :element-type 'fixnum :initial-element 0))
         (next (make-array 16 :element-type 'fixnum :initial-element 0)))
    (loop for n from 0 below 144 do (setf (aref lengths n) 8))
    (loop for n from 144 below 256 do (setf (aref lengths n) 9))
    (loop for n from 256 below 280 do (setf (aref lengths n) 7))
    (loop for n from 280 below 288 do (setf (aref lengths n) 8))
    (loop for n from 0 below 288 do (incf (aref bl-count (aref lengths n))))
    (%fill-canonical-codes lengths codes 288 bl-count next)
    (values codes lengths)))

;;; The static trees are built eagerly at load time.  Lazy check-then-act
;;; initialization of these four globals was a data race under concurrent
;;; compression (several sequential SETFs; a reader between them observed a
;;; half-initialized tree).  Eager construction makes the race impossible.
(multiple-value-bind (codes lengths)
    (compute-static-lit-tree)
  (defparameter +static-lit-codes+ codes)
  (defparameter +static-lit-lengths+ lengths))

(defparameter +static-dist-codes+
  (let ((codes (make-array 30 :element-type 'fixnum)))
    (dotimes (n 30)
      (setf (aref codes n) (reverse-bits n 5)))
    codes))

(defparameter +static-dist-lengths+
  (make-array 30 :element-type 'fixnum :initial-element 5))

(declaim (type (simple-array fixnum (*))
               +static-lit-codes+ +static-lit-lengths+
               +static-dist-codes+ +static-dist-lengths+))

(defun ensure-static-trees ()
  "Return the static Huffman trees (built at load time)."
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

;;; Reusable scratch for BUILD-HUFFMAN-CODES.  The compressor calls the
;;; builder three times per block; handing it preallocated arrays keeps the
;;; hot path free of large allocations.
(defstruct (huff-work
            (:conc-name hw-)
            (:constructor make-huff-work))
  (heap nil :type (simple-array fixnum (*)))
  (dad nil :type (simple-array fixnum (*)))
  (depth nil :type (simple-array fixnum (*)))
  (node-length nil :type (simple-array fixnum (*)))
  (bl-count nil :type (simple-array fixnum (*)))
  (next nil :type (simple-array fixnum (*))))
;;; already-typed; the builder's LOCAL declarations below are what matter

(defun make-standard-huff-work ()
  (make-huff-work
   :heap (make-array +heap-size+ :element-type 'fixnum)
   :dad (make-array +heap-size+ :element-type 'fixnum)
   :depth (make-array +heap-size+ :element-type 'fixnum)
   :node-length (make-array +heap-size+ :element-type 'fixnum)
   :bl-count (make-array (1+ +max-code-length+) :element-type 'fixnum
                         :initial-element 0)
   :next (make-array (1+ +max-code-length+) :element-type 'fixnum
                     :initial-element 0)))

(declaim (inline %heap-smaller-p))

(defun %heap-smaller-p (freq depth n m)
  "Heap order: smaller frequency wins, ties broken by smaller depth."
  (declare (type (simple-array fixnum (*)) freq depth)
           (type fixnum n m)
           (optimize (speed 3) (safety 0)))
  (let ((fn (aref freq n)) (fm (aref freq m)))
    (declare (type fixnum fn fm))
    (or (< fn fm) (and (= fn fm) (<= (aref depth n) (aref depth m))))))

(defun %heap-sift-down (freq depth heap heap-len k)
  "Restore heap order below K (1-based)."
  (declare (type (simple-array fixnum (*)) freq depth heap)
           (type fixnum heap-len k)
           (optimize (speed 3) (safety 0)))
  (let ((v (aref heap k)) (kk k))
    (declare (type fixnum v kk))
    (loop for j fixnum = (ash kk 1) then (ash j 1) do
      (when (and (< j heap-len)
                 (%heap-smaller-p freq depth (aref heap (1+ j)) (aref heap j)))
        (incf j))
      (when (or (> j heap-len) (%heap-smaller-p freq depth v (aref heap j)))
        (return))
      (setf (aref heap kk) (aref heap j) kk j))
    (setf (aref heap kk) v))
  nil)

(defun %heap-pop (freq depth heap heap-len)
  "Remove and return the heap top; returns (VALUES TOP NEW-LEN)."
  (declare (type (simple-array fixnum (*)) freq depth heap)
           (type fixnum heap-len)
           (optimize (speed 3) (safety 0)))
  (let ((top (aref heap 1)))
    (declare (type fixnum top))
    (setf (aref heap 1) (aref heap heap-len)
          heap-len (1- heap-len))
    (%heap-sift-down freq depth heap heap-len 1)
    (values top heap-len)))

(defun %heap-init-leaves (freq elems depth heap leaf-lengths)
  "Push nonzero-frequency leaves, force two codes, heapify.  Returns
(VALUES HEAP-LEN MAX-CODE)."
  (declare (type (simple-array fixnum (*)) freq depth heap leaf-lengths)
           (type fixnum elems)
           (optimize (speed 3) (safety 0)))
  (let ((heap-len 0) (max-code -1))
    (declare (type fixnum heap-len max-code))
    (loop for n from 0 below elems do
      (if (plusp (aref freq n))
          (setf heap-len (1+ heap-len)
                (aref heap heap-len) n
                max-code n
                (aref depth n) 0)
          (setf (aref leaf-lengths n) 0)))
    ;; force at least two codes of non-zero frequency (pkzip requirement)
    (loop while (< heap-len 2) do
      (let ((node (if (< max-code 2) (incf max-code) 0)))
        (declare (type fixnum node))
        (setf heap-len (1+ heap-len)
              (aref heap heap-len) node
              (aref freq node) 1
              (aref depth node) 0)))
    (loop for n from (floor heap-len 2) downto 1 do
      (%heap-sift-down freq depth heap heap-len n))
    (values heap-len max-code)))

(defun %heap-combine-trees (freq depth dad heap heap-len heap-max node)
  "Combine least-frequent nodes into internal ones; returns (VALUES
HEAP-LEN HEAP-MAX) with the tree root staged at HEAP[HEAP-MAX]."
  (declare (type (simple-array fixnum (*)) freq depth dad heap)
           (type fixnum heap-len heap-max node)
           (optimize (speed 3) (safety 0)))
  (loop while (>= heap-len 2) do
    (multiple-value-bind (n nl) (%heap-pop freq depth heap heap-len)
      (setf heap-len nl)
      (let ((m (aref heap 1)))
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
        (%heap-sift-down freq depth heap heap-len 1)
        (incf node))))
  (setf heap-max (1- heap-max)
        (aref heap heap-max) (aref heap 1))
  (values heap-len heap-max))

(defun %gen-bit-lengths (freq depth dad heap heap-max max-code max-length
                         bl-count node-length)
  "Compute lengths from the tree (gen_bitlen); returns OVERFLOW beyond
MAX-LENGTH."
  (declare (type (simple-array fixnum (*)) freq depth dad heap bl-count
                 node-length)
           (type fixnum heap-max max-code max-length)
           (optimize (speed 3) (safety 0)))
  (setf (aref node-length (aref heap heap-max)) 0)
  (let ((overflow 0))
    (declare (type fixnum overflow))
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
    overflow))

(defun %fix-length-overflow (bl-count max-length overflow heap max-code
                             node-length)
  "zlib bl_count adjustment forcing the code into MAX-LENGTH bits, then
recompute lengths scanning the heap in increasing frequency."
  (declare (type (simple-array fixnum (*)) bl-count heap node-length)
           (type fixnum max-length overflow max-code)
           (optimize (speed 3) (safety 0)))
  (loop while (> overflow 0) do
    (let ((bits (1- max-length)))
      (declare (type fixnum bits))
      (loop while (zerop (aref bl-count bits)) do (decf bits))
      (decf (aref bl-count bits))
      (incf (aref bl-count (1+ bits)) 2)
      (decf (aref bl-count max-length))
      (decf overflow 2)))
  (let ((h +heap-size+))
    (declare (type fixnum h))
    (loop for bits from max-length downto 1 do
      (let ((n (aref bl-count bits)))
        (declare (type fixnum n))
        (loop while (plusp n) do
          (decf h)
          (let ((m (aref heap h)))
            (declare (type fixnum m))
            (when (<= m max-code)
              (setf (aref node-length m) bits)
              (decf n))))))))

(defun %copy-leaf-lengths (freq leaf-lengths node-length elems max-code)
  "Copy meaningful NODE-LENGTHs out; unused symbols stay at length 0."
  (declare (type (simple-array fixnum (*)) freq leaf-lengths node-length)
           (type fixnum elems max-code)
           (optimize (speed 3) (safety 0)))
  (loop for n from 0 below elems do
    (when (plusp (aref freq n))
      (setf (aref leaf-lengths n) (aref node-length n)))))

(defun build-huffman-codes (freq elems max-length &optional
                                         work out-lengths out-codes)
  "Given FREQ compute a length-limited canonical Huffman code.  Returns
(VALUES LENGTHS CODES MAX-CODE); WORK/OUT-LENGTHS/OUT-CODES, when given,
are reused scratch and result arrays.  FREQ needs room past ELEMS: the
heap stages internal nodes there."
  (declare (type (simple-array fixnum (*)) freq)
           (type fixnum elems max-length)
           (optimize (speed 3) (safety 0)))
  (let* ((work (or work (make-standard-huff-work)))
         (heap (hw-heap work)) (dad (hw-dad work)) (depth (hw-depth work))
         (node-length (hw-node-length work))
         (leaf-lengths (or out-lengths (make-array elems :element-type 'fixnum)))
         (codes (or out-codes (make-array elems :element-type 'fixnum)))
         (bl-count (hw-bl-count work)) (next (hw-next work))
         (heap-len 0) (heap-max +heap-size+) (max-code -1))
    (declare (type (simple-array fixnum (*)) heap dad depth node-length
                   leaf-lengths codes bl-count next)
             (type fixnum heap-len heap-max max-code))
    (fill bl-count 0)
    (fill next 0)
    (multiple-value-bind (hl mc)
        (%heap-init-leaves freq elems depth heap leaf-lengths)
      (setf heap-len hl max-code mc))
    (multiple-value-bind (chl chm)
        (%heap-combine-trees freq depth dad heap heap-len heap-max elems)
      (setf heap-len chl heap-max chm))
    (let ((overflow (%gen-bit-lengths freq depth dad heap heap-max max-code
                                      max-length bl-count node-length)))
      (declare (type fixnum overflow))
      (when (plusp overflow)
        (%fix-length-overflow bl-count max-length overflow
                              heap max-code node-length))
      (%copy-leaf-lengths freq leaf-lengths node-length elems max-code)
      (%fill-canonical-codes leaf-lengths codes (1+ max-code) bl-count next)
      (values leaf-lengths codes max-code))))
