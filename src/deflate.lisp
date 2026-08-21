(in-package #:cl-newzlib)

;;; DEFLATE compressor (RFC 1951).
;;;
;;; Pipeline: LZ77 match finding over the input with a 32 KiB sliding window
;;; (hash chains), token stream in literal/length-distance form, then block
;;; emission.  For each block we build Huffman codes from the token
;;; frequencies and pick the cheapest of stored / fixed / dynamic encoding.
;;;
;;; Positions stored in the hash chains are 16-bit residues (position mod
;;; 2^16), exactly like zlib's u16 head/prev tables: chains are walked by
;;; converting a residue back to an absolute candidate with
;;; CAND = POS - ((POS - RESIDUE) mod 2^16), and any candidate whose distance
;;; exceeds the window (or the current position) terminates the walk.  Stale
;;; entries left over from earlier inputs can only produce spurious byte
;;; comparisons -- every match is verified against the actual input bytes --
;;; never wrong output, so the tables need not be cleared between calls.
;;;
;;; All large per-call scratch (hash tables, token arrays, frequency counts,
;;; Huffman work arrays, the output bit-writer) lives in an LZ77-SCRATCH
;;; record recycled through a small lock-protected pool, keeping allocation
;;; per compression close to zero.

(defconstant +window-size+ 32768)
(defconstant +hash-size+ 32768)
(defconstant +window-mask+ (1- +window-size+))
(defconstant +hash-mask+ (1- +hash-size+))
(defconstant +min-match+ 3)
(defconstant +max-match+ 258)
(defconstant +max-dist+ 32768)
(defconstant +max-stored-block+ 65535)

;;; Compression-level tuning, mirroring zlib's configuration_table
;;; (good_length, max_lazy, nice_length, max_chain).  Lazy matching is only
;;; used for levels >= 4 (deflate_slow); levels 1-3 are greedy (deflate_fast).
(defun good-length (level)
  (case level
    (0 0)
    ((1 2 3) 4)
    ((4 5) 8)
    ((6 7) 8)
    (8 32)
    (9 32)
    (otherwise (error 'newzlib-parameter-error
                      :detail (format nil "invalid compression level ~A" level)))))

(defun lazy-length (level)
  (case level
    (0 0)
    (1 4)
    (2 5)
    (3 6)
    (4 4)
    (5 16)
    (6 16)
    (7 32)
    (8 128)
    (9 258)
    (otherwise (error 'newzlib-parameter-error
                      :detail (format nil "invalid compression level ~A" level)))))

(defun nice-length (level)
  (case level
    (0 0)
    (1 8)
    (2 16)
    (3 32)
    (4 16)
    (5 32)
    (6 128)
    (7 128)
    (8 258)
    (9 258)
    (otherwise (error 'newzlib-parameter-error
                      :detail (format nil "invalid compression level ~A" level)))))

(defun chain-limit (level)
  (case level
    (0 0)
    (1 4)
    (2 8)
    (3 32)
    (4 16)
    (5 32)
    (6 128)
    (7 256)
    (8 1024)
    (9 4096)
    (otherwise (error 'newzlib-parameter-error
                      :detail (format nil "invalid compression level ~A" level)))))

;;; Return-type proclamations keep the callers' token bookkeeping fully
;;; unboxed (the multiple values feed straight into fixnum arithmetic).
(declaim (ftype (function * (values fixnum &optional)) insert-string)
         (ftype (function * (values fixnum fixnum &optional)) longest-match))

(declaim (inline hash-3))
(defun hash-3 (input pos)
  "Hash the three bytes at INPUT[POS..POS+2] into HASH-SIZE buckets."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum pos))
  (logand (logxor (aref input pos)
                  (ash (aref input (1+ pos)) 5)
                  (ash (aref input (+ pos 2)) 10))
          +hash-mask+))

(declaim (inline insert-string))
(defun insert-string (input pos head prev)
  "Insert POS into the hash chain for its 3-byte hash.  Returns the previous
head of the chain (a 16-bit position residue, like zlib's INSERT_STRING
match_head)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 16) (*)) head prev)
           (type fixnum pos))
  (let ((h (hash-3 input pos))
        (res (ldb (byte 16 0) pos)))
    (declare (type fixnum h res))
    (let ((old (aref head h)))
      (setf (aref prev (logand pos +window-mask+)) old
            (aref head h) res)
      old)))

(defun longest-match (input base pos end first-res head prev max-chain nice good best-len)
  "Find the longest match for the string starting at INPUT[POS], ignoring
matches no longer than BEST-LEN (zlib seeds this with the pending lazy match
length).  FIRST-RES is the first chain entry as a 16-bit position residue
(the hash head captured before POS was inserted, so it never equals POS).
BASE is the raw SAP of INPUT on SBCL (pinned by the caller); elsewhere it is
ignored and ordinary array accesses are used.  Returns (VALUES LENGTH
DISTANCE).  Overlapping matches are allowed (the byte being matched at
POS+LEN is the byte DISTANCE positions back), so runs longer than the
distance are handled."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 16) (*)) head prev)
           (type fixnum pos end first-res max-chain nice good best-len))
  #+sbcl
  (declare (type sb-sys:system-area-pointer base))
  #-sbcl
  (declare (ignore base))
  (labels ((extend-match (cand start-len)
             "Longest run INPUT[POS+LEN..] == INPUT[CAND+LEN..] starting at
START-LEN, bounded by END and +MAX-MATCH+."
             (declare (type fixnum cand start-len))
             #+sbcl
             (let ((len start-len))
               (declare (type fixnum len))
               ;; compare four bytes at a time; INPUT is pinned by RUN-LZ77
               ;; for the whole search and the unaligned reads stay inside END
               (loop while (and (< (+ pos 4 len) end)
                                (< len +max-match+)
                                (= (sb-sys:sap-ref-32 base (+ pos len))
                                   (sb-sys:sap-ref-32 base (+ cand len))))
                     do (incf len 4))
               (loop while (and (< (+ pos len) end)
                                (< len +max-match+)
                                (= (aref input (+ pos len))
                                   (aref input (+ cand len))))
                     do (incf len))
               len)
             #-sbcl
             (let ((len start-len))
               (declare (type fixnum len))
               (loop while (and (< (+ pos len) end)
                                (< len +max-match+)
                                (= (aref input (+ pos len))
                                   (aref input (+ cand len))))
                     do (incf len))
               len))
           (walk ()
             (let ((best-dist 0)
                   (chain 0)
                   (res first-res))
               (declare (type fixnum best-dist chain res))
               (when (>= best-len good)
                 (setf max-chain (ash max-chain -2)))
               ;; INPUT[POS..POS+1] never change across candidates, so hoist
               ;; them out of the walk; only the best-len-dependent and
               ;; candidate bytes stay inline.
               (let ((p0 (aref input pos))
                     (p1 (aref input (1+ pos))))
                 (declare (type fixnum p0 p1))
                 (loop
                   ;; an empty-bucket or exhausted-chain sentinel ends the
                   ;; walk immediately
                   (when (= res #xFFFF) (return))
                   ;; convert the residue to the most recent absolute
                   ;; position; a zero distance or one past the window (or
                   ;; past the start of the input) ends the walk
                   (let ((delta (logand (- pos res) #xFFFF)))
                     (declare (type fixnum delta))
                     ;; DELTA = 0 flags a residue aliased from 64K back;
                     ;; DELTA > POS means a stale link from an earlier input
                     ;; (PREV is deliberately not cleared) reaching past the
                     ;; start; both end the walk, as does the window check
                     (when (or (zerop delta) (> delta +max-dist+) (> delta pos))
                       (return))
                     (when (>= chain max-chain) (return))
                     (incf chain)
                     (let ((cand (- pos delta)))
                       (declare (type fixnum cand))
                       (when (and (= p0 (aref input cand))
                                  (= p1 (aref input (1+ cand)))
                                  (< (+ pos best-len) end)
                                  (= (aref input (+ pos best-len)) (aref input (+ cand best-len)))
                                  (= (aref input (+ pos best-len -1)) (aref input (+ cand best-len -1))))
                         (let ((distance delta))
                           (declare (type fixnum distance))
                           (let ((len (extend-match cand 2)))
                             (declare (type fixnum len))
                             (when (> len best-len)
                               (setf best-len len
                                     best-dist distance)
                               (when (>= len nice) (return)))))))
                     (setf res (aref prev (logand (- pos delta) +window-mask+)))))
                 (values best-len best-dist)))))
    (declare (inline extend-match walk))
    (walk)))

;;; ------------------------------------------------------------------
;;; Scratch pool
;;; ------------------------------------------------------------------
;;;
;;; Hash chains, token arrays and frequency/Huffman scratch are large but
;;; short-lived; reallocating them per call turns small compressions into
;;; pure GC pressure (~600 KB consed per 512-byte object).  A bounded,
;;; lock-protected free list recycles them across calls.

(defstruct (lz77-scratch
            (:conc-name lzs-)
            (:constructor %make-lzs))
  (head nil :type (simple-array (unsigned-byte 16) (*)))   ; hash heads
  (prev nil :type (simple-array (unsigned-byte 16) (*)))   ; chain links
  (sym nil :type (simple-array (unsigned-byte 16) (*)))    ; literal/length syms
  (distc nil :type (simple-array (unsigned-byte 16) (*)))  ; distance codes
  (el nil :type (simple-array (unsigned-byte 16) (*)))     ; match lengths
  (ed nil :type (simple-array (unsigned-byte 16) (*)))     ; match distances
  (lit-freq nil :type (simple-array fixnum (*)))
  (dist-freq nil :type (simple-array fixnum (*)))
  (bl-sym nil :type (simple-array fixnum (*)))
  (bl-extra nil :type (simple-array fixnum (*)))
  (bl-freq nil :type (simple-array fixnum (*)))
  (lit-lengths nil :type (simple-array fixnum (*)))
  (lit-codes nil :type (simple-array fixnum (*)))
  (dist-lengths nil :type (simple-array fixnum (*)))
  (dist-codes nil :type (simple-array fixnum (*)))
  (bl-lengths nil :type (simple-array fixnum (*)))
  (bl-codes nil :type (simple-array fixnum (*)))
  (work nil :type (or null huff-work))
  (writer nil :type (or null bit-writer)))

(defparameter *scratch-pool-max* 4)
(defvar *scratch-pool* '())
#+sb-thread
(defvar *scratch-lock* (sb-thread:make-mutex :name "cl-newzlib scratch pool"))
#-sb-thread
(defparameter *scratch-lock* nil)

(defmacro with-scratch-lock (&body body)
  #+sb-thread `(sb-thread:with-mutex (*scratch-lock*) ,@body)
  #-sb-thread `(locally ,@body))

(defun make-u16-vector (n)
  (make-array n :element-type '(unsigned-byte 16)))

(defun make-fixnum-vector (n)
  (make-array n :element-type 'fixnum))

(defun acquire-lz77-scratch (token-size)
  "Get a LZ77-SCRATCH whose token arrays hold at least TOKEN-SIZE entries.
The hash heads are filled with the empty-bucket sentinel (#xFFFF); chain
links may keep stale values -- real chains always walk back through entries
written during the current input before they can reach them, and any bogus
candidate is byte-verified like a real one, so stale links cost only the
occasional wasted comparison."
  (let ((s (with-scratch-lock (pop *scratch-pool*))))
    (unless s
      (setf s (%make-lzs
               :head (make-u16-vector +hash-size+)
               :prev (make-u16-vector +window-size+)
               :sym (make-u16-vector token-size)
               :distc (make-u16-vector token-size)
               :el (make-u16-vector token-size)
               :ed (make-u16-vector token-size)
               :lit-freq (make-fixnum-vector +heap-size+)
               :dist-freq (make-fixnum-vector +heap-size+)
               :bl-sym (make-fixnum-vector 320)
               :bl-extra (make-fixnum-vector 320)
               :bl-freq (make-fixnum-vector +heap-size+)
               :lit-lengths (make-fixnum-vector +l-codes+)
               :lit-codes (make-fixnum-vector +l-codes+)
               :dist-lengths (make-fixnum-vector +d-codes+)
               :dist-codes (make-fixnum-vector +d-codes+)
               :bl-lengths (make-fixnum-vector +bl-codes+)
               :bl-codes (make-fixnum-vector +bl-codes+)
               :work (make-standard-huff-work)
               :writer (make-bit-writer 4096))))
    (when (< (length (lzs-sym s)) token-size)
      (setf (lzs-sym s) (make-u16-vector token-size)
            (lzs-distc s) (make-u16-vector token-size)
            (lzs-el s) (make-u16-vector token-size)
            (lzs-ed s) (make-u16-vector token-size)))
    (fill (lzs-head s) #xFFFF)
    ;; PREV is left stale on purpose: real chains terminate inside the
    ;; current input's entries before reaching stale links in most cases,
    ;; and stale links are always byte-verified like real ones.
    s))

(defun release-lz77-scratch (s)
  (declare (type lz77-scratch s))
  (with-scratch-lock
    (when (< (length *scratch-pool*) *scratch-pool-max*)
      (push s *scratch-pool*)))
  nil)

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

;;; Token emission helpers.
;;;
;;; These are macros over fixed local names so the LZ77 hot loop can update
;;; NSYM/EXTRA-BITS and write tokens without boxing any counters into
;;; closure cells.  They are only called from RUN-LZ77/%LZ77-SEARCH below,
;;; where every referenced name is bound.

(defmacro %emit-literal (p)
  ;; DIST/EL/ED are only read back for symbols > 256, so literals need not
  ;; store zeros into them (the arrays hold stale pool data there).
  `(let ((b (aref input ,p)))
     (setf (aref sym nsym) b)
     (incf (aref lit-freq b))
     (incf nsym)))

(defmacro %emit-match (len d)
  `(let* ((code (length-code ,len))
          (dcode (dist-code ,d)))
     (declare (type fixnum code dcode))
     (setf (aref sym nsym) (+ 257 code)
           (aref dist nsym) dcode
           (aref el nsym) ,len
           (aref ed nsym) ,d)
     (incf (aref lit-freq (+ 257 code)))
     (incf (aref dist-freq dcode))
     (incf extra-bits (+ (length-extra-bits code)
                         (dist-extra-bits dcode)))
     (incf nsym)))

(defmacro %insert-match-interior (mpos mlen start)
  ;; rolling 3-byte hash: keep INPUT[Q..Q+2] in locals so each consecutive
  ;; insertion reads only one fresh byte instead of recomputing HASH-3's
  ;; three loads from scratch.
  `(let ((limit (min (+ ,mpos ,mlen) (- end 2))))
     (declare (type fixnum limit))
     (loop for q from ,start below limit
           for a = (aref input q) then b
           for b = (aref input (1+ q)) then c
           for c = (aref input (+ q 2))
         do (let ((h (logand (logxor a (ash b 5) (ash c 10))
                             +hash-mask+))
                  (qres (ldb (byte 16 0) q)))
              (declare (type fixnum h qres))
              ;; link Q after the current chain head, like INSERT-STRING
              (setf (aref prev (logand q +window-mask+)) (aref head h)
                    (aref head h) qres)))))

(defun %lz77-search (input base start end nice good max-chain max-lazy lazy-p
                     sym dist el ed head prev lit-freq dist-freq)
  "LZ77 tokenization core; see RUN-LZ77.  BASE is INPUT's SAP on SBCL
(where INPUT is pinned for the duration), ignored elsewhere.  Returns
(VALUES NSYM EXTRA-BITS)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed head prev)
           (type (simple-array fixnum (*)) lit-freq dist-freq)
           (type fixnum start end nice good max-chain max-lazy))
  #+sbcl
  (declare (type sb-sys:system-area-pointer base))
  #-sbcl
  (declare (type null base)
           (ignore base))
  (let ((nsym 0)
        (extra-bits 0)
        (pos start))
    (declare (type fixnum nsym extra-bits pos))
    (if lazy-p
        ;; lazy matching: defer each match one position (zlib deflate_slow)
        (let ((have-pending nil)
              (pending-len 0)
              (pending-dist 0)
              (pending-pos 0))
          (declare (type fixnum pending-len pending-dist pending-pos))
          (loop while (< pos end) do
            (if (< (- end pos) +min-match+)
                ;; no room to search for a new match: flush any pending
                ;; match and emit the remaining bytes as literals
                (progn
                  (when have-pending
                    (if (>= pending-len +min-match+)
                        (progn
                          (%emit-match pending-len pending-dist)
                          (setf pos (+ pending-pos pending-len)))
                        (progn
                          (%emit-literal pending-pos)
                          (setf pos (1+ pending-pos))))
                    (setf have-pending nil))
                  (loop while (< pos end) do
                    (%emit-literal pos)
                    (incf pos)))
                (progn
                  (let ((cand (insert-string input pos head prev)))
                    (let ((mlen 0) (mdist 0))
                      (declare (type fixnum mlen mdist))
                      (when (and (/= cand #xFFFF)
                                 (or (not have-pending)
                                     (< pending-len max-lazy)))
                        (multiple-value-bind (len d)
                            (longest-match input base pos end cand head prev
                                           max-chain nice good
                                           (if have-pending
                                               pending-len
                                               (1- +min-match+)))
                          (setf mlen len mdist d)))
                      (cond
                        ;; the pending match is at least as good: emit it
                        ((and have-pending
                              (>= pending-len +min-match+)
                              (<= mlen pending-len))
                         (%emit-match pending-len pending-dist)
                         (%insert-match-interior pending-pos pending-len
                                                 (+ pending-pos 2))
                         (setf pos (+ pending-pos pending-len)
                               have-pending nil))
                        ;; there is a pending position: output its byte as a
                        ;; literal, keep the current (longer) match pending
                        (have-pending
                         (%emit-literal (1- pos))
                         (incf pos)
                         (setf pending-len mlen
                               pending-dist mdist
                               pending-pos (1- pos)))
                        ;; nothing pending: wait for the next step to decide
                        (t
                         (setf have-pending t
                               pending-len mlen
                               pending-dist mdist
                               pending-pos pos)
                         (incf pos))))))))
          ;; flush any pending match at end of input
          (when have-pending
            (if (>= pending-len +min-match+)
                (%emit-match pending-len pending-dist)
                (%emit-literal pending-pos))))
        ;; greedy matching (zlib deflate_fast)
        (loop while (< pos end) do
          (if (< (- end pos) +min-match+)
              (progn
                (%emit-literal pos)
                (incf pos))
              (progn
                (let ((cand (insert-string input pos head prev)))
                  (if (= cand #xFFFF)
                      ;; empty bucket: no candidate, emit a literal
                      (progn
                        (%emit-literal pos)
                        (incf pos))
                      (multiple-value-bind (len d)
                          (longest-match input base pos end cand head prev
                                         max-chain nice good (1- +min-match+))
                        (if (>= len +min-match+)
                            (progn
                              (%emit-match len d)
                              (when (<= len max-lazy)
                                (%insert-match-interior pos len (1+ pos)))
                              (incf pos len))
                            (progn
                              (%emit-literal pos)
                              (incf pos))))))))))
    (values nsym extra-bits)))

(defun run-lz77 (input start end level sym dist el ed lit-freq dist-freq
                 &optional (head (make-array +hash-size+
                                             :element-type '(unsigned-byte 16)
                                             :initial-element #xFFFF))
                           (prev (make-array +window-size+
                                             :element-type '(unsigned-byte 16)
                                             :initial-element #xFFFF)))
  "Run LZ77 over INPUT[START,END), filling SYM/DIST/EL/ED (sized to the
input length) and the symbol frequency vectors.  Returns (VALUES NSYM
EXTRA-BITS) where EXTRA-BITS is the total number of length/distance extra
bits across all matches.  Levels 1-3 use greedy matching (deflate_fast),
levels 4-9 use lazy matching (deflate_slow) which defers each match one
position and only adopts it if no longer match starts on the next byte."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed head prev)
           (type (simple-array fixnum (*)) lit-freq dist-freq)
           (type fixnum start end level))
  ;; the raw SAP of INPUT is only valid while INPUT cannot move, so pin it
  ;; once for the entire search instead of per candidate comparison
  #+sbcl
  (sb-sys:with-pinned-objects (input)
    (multiple-value-bind (nsym extra-bits)
        (%lz77-search input (sb-sys:vector-sap input) start end
                      (nice-length level) (good-length level)
                      (chain-limit level) (lazy-length level) (> level 3)
                      sym dist el ed head prev lit-freq dist-freq)
      (values (finish-token-stream sym el ed lit-freq nsym) extra-bits)))
  #-sbcl
  (multiple-value-bind (nsym extra-bits)
      (%lz77-search input nil start end
                    (nice-length level) (good-length level)
                    (chain-limit level) (lazy-length level) (> level 3)
                    sym dist el ed head prev lit-freq dist-freq)
    (values (finish-token-stream sym el ed lit-freq nsym) extra-bits)))

(defun finish-token-stream (sym el ed lit-freq nsym)
  "Append the end-of-block symbol at NSYM and bump its frequency."
  (declare (type (simple-array (unsigned-byte 16) (*)) sym el ed)
           (type (simple-array fixnum (*)) lit-freq)
           (type fixnum nsym)
           (optimize (speed 3) (safety 0)))
  (setf (aref sym nsym) 256
        (aref el nsym) 0
        (aref ed nsym) 0)
  (incf (aref lit-freq 256))
  (incf nsym)
  nsym)

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
  "Bit count of stored blocks covering N bytes given WRITER's current
alignment.  Blocks after the first start at a byte boundary."
  (let ((pad (mod (- 8 (+ (logand 7 (bw-nbits writer)) 3)) 8)))
    (if (<= n +max-stored-block+)
        (+ 3 pad 32 (* 8 n))
        (+ (* 8 n)
           (+ 3 pad 32)
           (* (+ 3 32) (1- (ceiling n +max-stored-block+)))))))

(defun emit-stored-blocks (writer input start end)
  "Emit INPUT[START,END) as stored blocks, splitting at +MAX-STORED-BLOCK+
bytes per block."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end))
  (if (< start end)
      (loop for s from start below end by +max-stored-block+
            do (let ((e (min end (+ s +max-stored-block+))))
                 (emit-stored-block writer input s e (>= e end))))
      (emit-stored-block writer input start end t)))

(defun emit-stored-block (writer input start end bfinal)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
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
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed)
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
           (type (simple-array (unsigned-byte 16) (*)) sym dist)
           (type (simple-array fixnum (*)) len-array dist-len-array)
           (type fixnum nsym extra-bits))
  (let ((bits extra-bits))
    (declare (type fixnum bits))
    (loop for i below nsym do
      (let ((s (aref sym i)))
        (incf bits (aref len-array s))
        (when (> s 256)
          (incf bits (aref dist-len-array (aref dist i))))))
    bits))

(defun data-bits/dynamic-and-fixed (sym dist nsym dyn-lens fixed-lens
                                     dist-dyn-lens dist-fixed-lens extra-bits)
  "Like DATA-BITS twice: returns (VALUES DYN-BITS FIXED-BITS) computing both
size estimates in a single pass over the tokens."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 16) (*)) sym dist)
           (type (simple-array fixnum (*)) dyn-lens fixed-lens
                 dist-dyn-lens dist-fixed-lens)
           (type fixnum nsym extra-bits))
  (let ((dyn extra-bits)
        (fixed extra-bits))
    (declare (type fixnum dyn fixed))
    (loop for i below nsym do
      (let ((s (aref sym i)))
        (declare (type fixnum s))
        (incf dyn (aref dyn-lens s))
        (incf fixed (aref fixed-lens s))
        (when (> s 256)
          (let ((d (aref dist i)))
            (declare (type fixnum d))
            (incf dyn (aref dist-dyn-lens d))
            (incf fixed (aref dist-fixed-lens d))))))
    (values dyn fixed)))

(defun scan-code-lengths (lengths n bl-sym bl-extra bl-freq nbl)
  "RLE-encode LENGTHS[0..N) into code-length symbols (RFC 1951 3.2.7),
appending to BL-SYM/BL-EXTRA starting at NBL and counting frequencies into
BL-FREQ.  Returns (VALUES NBL EXTRA-BITS)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (*)) lengths bl-sym bl-extra bl-freq)
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
      (iterate:iterate
        (iterate:for idx from 0 below n)
        (iterate:for curlen = nextlen)
        (setf nextlen (if (< idx (1- n)) (aref lengths (1+ idx)) -1))
        (incf count)
        (unless (and (< count max-count) (= curlen nextlen))
          (cond
            ((< count min-count)
             (iterate:iterate (iterate:repeat count)
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
                (t (setf max-count 7 min-count 4)))))
      (values nbl extra-bits))))

(defun emit-dynamic-block (writer sym dist el ed nsym bfinal
                            lit-codes lit-lengths dist-codes dist-lengths
                            bl-sym bl-extra bl-codes bl-lengths nbl
                            hlit hdist hclen)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed)
           (type (simple-array fixnum (*)) lit-codes lit-lengths dist-codes
                              dist-lengths bl-sym bl-extra bl-codes bl-lengths)
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

(defun reset-bit-writer (writer size)
  "Point WRITER at a buffer of at least SIZE octets and clear its state."
  (declare (type fixnum size))
  (when (< (bw-size writer) size)
    (setf (bw-buffer writer) (make-octet-buffer size)
          (bw-size writer) size))
  (setf (bw-pos writer) 0
        (bw-accum writer) 0
        (bw-nbits writer) 0)
  writer)

(defun deflate-into-writer (input start end writer level)
  "Compress INPUT[START,END) into WRITER as one DEFLATE stream.  Level 0
emits stored blocks; higher levels pick the cheapest block encoding."
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end level))
  (let ((n (- end start)))
    (if (zerop level)
        (emit-stored-blocks writer input start end)
        (let ((scratch (acquire-lz77-scratch (1+ n))))
          (unwind-protect
               (progn
                 (fill (lzs-lit-freq scratch) 0)
                 (fill (lzs-dist-freq scratch) 0)
                 (fill (lzs-bl-freq scratch) 0)
                  (multiple-value-bind (nsym extra-bits)
                      (run-lz77 input start end level
                                (lzs-sym scratch) (lzs-distc scratch)
                                (lzs-el scratch) (lzs-ed scratch)
                                (lzs-lit-freq scratch) (lzs-dist-freq scratch)
                                (lzs-head scratch) (lzs-prev scratch))
                    (if (<= n 1024)
                        ;; Tiny input: dynamic Huffman trees rarely pay for
                        ;; their construction cost here, so pick between the
                        ;; two encodings that need no tree building.
                        (let ((stored-size (stored-block-bits writer n))
                              (fixed-size (+ 3 (data-bits (lzs-sym scratch)
                                                          (lzs-distc scratch)
                                                          nsym
                                                          +static-lit-lengths+
                                                          +static-dist-lengths+
                                                          extra-bits))))
                          (ensure-static-trees)
                          (if (<= stored-size fixed-size)
                              (emit-stored-blocks writer input start end)
                              (emit-fixed-block writer (lzs-sym scratch)
                                                (lzs-distc scratch)
                                                (lzs-el scratch) (lzs-ed scratch)
                                                nsym t)))
                        (multiple-value-bind (lit-lengths lit-codes lit-max)
                            (build-huffman-codes (lzs-lit-freq scratch) 286 15
                                                 (lzs-work scratch)
                                                 (lzs-lit-lengths scratch)
                                                 (lzs-lit-codes scratch))
                     (declare (ignore lit-max))
                     (multiple-value-bind (dist-lengths dist-codes dist-max)
                         (build-huffman-codes (lzs-dist-freq scratch) 30 15
                                              (lzs-work scratch)
                                              (lzs-dist-lengths scratch)
                                              (lzs-dist-codes scratch))
                       (declare (ignore dist-max))
                       (let ((hlit (max 257 (1+ (loop for i from 285 downto 256
                                                    when (plusp (aref lit-lengths i))
                                                    return i))))
                             (hdist (max 1 (1+ (loop for i from 29 downto 0
                                                   when (plusp (aref dist-lengths i))
                                                   return i)))))
                         (multiple-value-bind (nbl1 bl-extra1)
                             (scan-code-lengths lit-lengths hlit
                                                (lzs-bl-sym scratch)
                                                (lzs-bl-extra scratch)
                                                (lzs-bl-freq scratch) 0)
                           (declare (ignore bl-extra1))
                           (multiple-value-bind (nbl2 bl-extra2)
                               (scan-code-lengths dist-lengths hdist
                                                  (lzs-bl-sym scratch)
                                                  (lzs-bl-extra scratch)
                                                  (lzs-bl-freq scratch) nbl1)
                             (multiple-value-bind (bl-lengths bl-codes bl-max)
                                 (build-huffman-codes (lzs-bl-freq scratch) 19 7
                                                      (lzs-work scratch)
                                                      (lzs-bl-lengths scratch)
                                                      (lzs-bl-codes scratch))
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
                                            (aref bl-lengths
                                                  (aref (lzs-bl-sym scratch) i))))
                                   (ensure-static-trees)
                                   (multiple-value-bind (dyn-bits fixed-bits)
                                       (data-bits/dynamic-and-fixed
                                        (lzs-sym scratch) (lzs-distc scratch) nsym
                                        lit-lengths +static-lit-lengths+
                                        dist-lengths +static-dist-lengths+
                                        extra-bits)
                                     (declare (ignorable dyn-bits fixed-bits))
                                     (let ((opt-size (+ 3 14 (* 3 hclen) bl-extra2 bl-code-bits dyn-bits))
                                         (fixed-size (+ 3 fixed-bits))
                                         (stored-size (stored-block-bits writer n)))
                                     (cond
                                       ((<= stored-size (min opt-size fixed-size))
                                        (emit-stored-blocks writer input start end))
                                       ((<= fixed-size opt-size)
                                        (emit-fixed-block writer (lzs-sym scratch)
                                                          (lzs-distc scratch)
                                                          (lzs-el scratch) (lzs-ed scratch)
                                                          nsym t))
                                       (t
                                        (emit-dynamic-block writer (lzs-sym scratch)
                                                            (lzs-distc scratch)
                                                            (lzs-el scratch) (lzs-ed scratch)
                                                            nsym t
                                                            lit-codes lit-lengths
                                                            dist-codes dist-lengths
                                                            (lzs-bl-sym scratch)
                                                            (lzs-bl-extra scratch)
                                                            bl-codes bl-lengths
                                                            nbl2 hlit hdist hclen))))))))))))))))
            (release-lz77-scratch scratch))))))

(defun deflate-raw (input &optional (start 0) (end (length input)) (level 6))
  "Compress INPUT[START,END) with raw DEFLATE (no zlib/gzip header) at LEVEL.
Returns a fresh octet vector."
  (check-compression-level level)
  (unless (typep input '(simple-array (unsigned-byte 8) (*)))
    (error 'newzlib-parameter-error :detail "input must be an (unsigned-byte 8) vector"))
  ;; worst-case output: stored blocks cost ~1% over the input plus headers
  (let* ((n (- end start))
         (bound (+ n (ash n -3) 256))
         (scratch (acquire-lz77-scratch (1+ n)))
         (writer (reset-bit-writer (lzs-writer scratch) bound)))
    (unwind-protect
         (progn
           (deflate-into-writer input start end writer level)
           (writer-bytes writer))
      (release-lz77-scratch scratch))))
