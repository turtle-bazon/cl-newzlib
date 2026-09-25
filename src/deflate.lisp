(in-package #:cl-newzlib)

;;; DEFLATE compressor (RFC 1951).
;;;
;;; Pipeline: LZ77 match finding over the input with a 32 KiB sliding window
;;; (hash chains), token stream in literal/length-distance form, then block
;;; emission.  For each block we build Huffman codes from the token
;;; frequencies and pick the cheapest of stored / fixed / dynamic encoding.
;;;
;;; Hash heads and chain links store absolute input positions (32-bit, with
;;; #xFFFFFFFF as the empty-bucket sentinel), so each chain step needs only
;;; a single window-limit comparison to stay inside the 32 KiB window.
;;; Tables are cleared on every acquire, keeping compression fully
;;; deterministic across calls.
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
;;; Head values are absolute positions (or #xFFFFFFFF when empty), which fit
;;; in a fixnum everywhere this code runs.
(declaim (ftype (function * (values fixnum &optional)) insert-string))

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
head of the chain (an absolute position, or #xFFFFFFFF for an empty bucket,
like zlib's INSERT_STRING match_head)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 32) (*)) head prev)
           (type fixnum pos))
  (let ((h (hash-3 input pos)))
    (declare (type fixnum h))
    (let ((old (aref head h)))
      (setf (aref prev (logand pos +window-mask+)) old
            (aref head h) pos)
      old)))

(declaim (inline %lm-limits %lm-candidate-ok-p %lm-extend %lm-bestpair-ok-p))
#+(and sbcl cl-newzlib-le)
(declaim (inline %lm-bestpair-sap))

(defun %lm-limits (pos end best good max-chain)
  "Per-search bounds: extension cap, window floor, chain budget (quartered
when the seed BEST already reaches GOOD, as in zlib).  The window floor
may go negative early in the input then it simply never triggers."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum pos end best good max-chain))
  (values (min (- end pos) +max-match+)
          (- pos +max-dist+)
          (if (>= best good) (ash max-chain -2) max-chain)))

(defun %lm-bestpair-ok-p (input pos cand best)
  "Best-len pair equal (portable byte form)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum pos cand best))
  (and (= (aref input (+ pos best)) (aref input (+ cand best)))
       (= (aref input (+ pos best -1)) (aref input (+ cand best -1)))))

#+(and sbcl cl-newzlib-le)
(defun %lm-bestpair-sap (isap pos cand best)
  "Best-len pair as one u16 compare per side (offsets BEST and BEST-1 are
adjacent, so a single 16-bit load covers both bytes)."
  (declare (optimize (speed 3) (safety 0))
           (type sb-sys:system-area-pointer isap)
           (type fixnum pos cand best))
  (= (sb-sys:sap-ref-16 isap (+ pos best -1))
     (sb-sys:sap-ref-16 isap (+ cand best -1))))

(defun %lm-candidate-ok-p (input pos end cand best p0 p1 isap)
  "Chain-candidate precheck with the best-len pair first (like C's
longest_match): it rejects ~98% of candidates, so the leading-byte loads
usually never execute.  ISAP is the pinned input SAP (SBCL only)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum pos end cand best p0 p1)
           #-(and sbcl cl-newzlib-le) (ignore isap))
  (and (< (+ pos best) end)
       #+(and sbcl cl-newzlib-le) (%lm-bestpair-sap isap pos cand best)
       #-(and sbcl cl-newzlib-le) (%lm-bestpair-ok-p input pos cand best)
       (= p0 (aref input cand))
       (= p1 (aref input (1+ cand)))))

(defun %lm-extend (input pos cand limit)
  "Full extension length from offset 2 via bulk compare (SIMD-backed; a
scalar-first probe was tried and netted negative, so kept SIMD)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum pos cand limit))
  (if (< 2 limit)
      (+ 2 (leading-equal-octets input (+ pos 2) (+ cand 2) (- limit 2)))
      2))

(defmacro longest-match (input pos end first-res head prev max-chain nice good best-len isap)
  "Longest match for INPUT[POS] over BEST-LEN (clamped >= 1 for the
best-len-1 lookahead).  FIRST-RES is the pre-insert chain head
(or #xFFFFFFFF; HEAD never evaluated); ISAP is the pinned input SAP
(SBCL only, for the u16 best-pair check).  Expands to (VALUES LENGTH
DISTANCE), inlining at both search sites; args side-effect-free."
  (let ((g-input (gensym "INPUT")) (g-pos (gensym "POS")) (g-end (gensym "END"))
        (g-res (gensym "RES")) (g-prev (gensym "PREV")) (g-max (gensym "MAX"))
        (g-nice (gensym "NICE")) (g-good (gensym "GOOD")) (g-best (gensym "BEST"))
        (g-dist (gensym "DIST")) (g-chain (gensym "CHAIN")) (g-isap (gensym "ISAP"))
        (g-p0 (gensym "P0")) (g-p1 (gensym "P1")) (g-cand (gensym "CAND")) (g-len (gensym "LEN")))
    `(let ((,g-input ,input) (,g-pos ,pos) (,g-end ,end) (,g-res ,first-res)
           (,g-prev ,prev) (,g-max ,max-chain) (,g-nice ,nice) (,g-good ,good)
           (,g-best (max ,best-len 1)) (,g-dist 0) (,g-chain 0) (,g-isap ,isap))
       (declare (optimize (speed 3) (safety 0))
                (type (simple-array (unsigned-byte 8) (*)) ,g-input)
                (type (simple-array (unsigned-byte 32) (*)) ,g-prev)
                (type fixnum ,g-pos ,g-end ,g-res ,g-max ,g-nice ,g-good
                      ,g-best ,g-dist ,g-chain))
       (multiple-value-bind (limit window maxc)
           (%lm-limits ,g-pos ,g-end ,g-best ,g-good ,g-max)
         (declare (type fixnum limit window maxc))
         (let ((,g-p0 (aref ,g-input ,g-pos)) (,g-p1 (aref ,g-input (1+ ,g-pos))))
           (declare (type fixnum ,g-p0 ,g-p1))
           (loop
             (when (or (= ,g-res #xFFFFFFFF) (< ,g-res window)
                       (>= ,g-chain maxc))
               (return))
             (incf ,g-chain)
             (let ((,g-cand ,g-res))
               (declare (type fixnum ,g-cand))
               (when (%lm-candidate-ok-p ,g-input ,g-pos ,g-end ,g-cand
                                         ,g-best ,g-p0 ,g-p1 ,g-isap)
                 (let ((,g-len (%lm-extend ,g-input ,g-pos ,g-cand limit)))
                   (declare (type fixnum ,g-len))
                   (when (> ,g-len ,g-best)
                     (setf ,g-best ,g-len ,g-dist (- ,g-pos ,g-cand))
                     (when (>= ,g-len ,g-nice) (return))))))
             (setf ,g-res (aref ,g-prev (logand ,g-res +window-mask+)))))
         (values ,g-best ,g-dist)))))

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
  (head nil :type (simple-array (unsigned-byte 32) (*)))   ; hash heads
  (prev nil :type (simple-array (unsigned-byte 32) (*)))   ; chain links
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

;;; Pool access must be serialized on any implementation that can run
;;; multiple threads; truly single-threaded images skip the lock.  An
;;; implementation not listed here that nonetheless supports threads would
;;; need its lock added below (until then it falls into the unlocked case,
;;; which is only safe for single-threaded use).
#+sb-thread
(defvar *scratch-lock* (sb-thread:make-mutex :name "cl-newzlib scratch pool"))
#+ccl
(defvar *scratch-lock* (ccl:make-lock "cl-newzlib scratch pool"))
#+lispworks
(defvar *scratch-lock* (mp:make-lock :name "cl-newzlib scratch pool"))
#+abcl
(defvar *scratch-lock* (threads:make-thread-lock "cl-newzlib scratch pool"))
#+(and ecl threads)
(defvar *scratch-lock* (mp:make-lock :name "cl-newzlib scratch pool"))
#+(and clasp threads)
(defvar *scratch-lock* (mp:make-lock :name "cl-newzlib scratch pool"))

(defmacro with-scratch-lock (&body body)
  #+sb-thread `(sb-thread:with-mutex (*scratch-lock*) ,@body)
  #+ccl `(ccl:with-lock-grabbed (*scratch-lock*) ,@body)
  #+lispworks `(mp:with-lock (*scratch-lock*) ,@body)
  #+abcl `(threads:with-thread-lock (*scratch-lock*) ,@body)
  #+(and ecl threads) `(mp:with-lock (*scratch-lock*) ,@body)
  #+(and clasp threads) `(mp:with-lock (*scratch-lock*) ,@body)
  #-(or sb-thread ccl lispworks abcl (and ecl threads) (and clasp threads))
  `(progn ,@body))

(defun make-u16-vector (n)
  (make-array n :element-type '(unsigned-byte 16)))

(defun make-u32-vector (n)
  (make-array n :element-type '(unsigned-byte 32)))

(defun make-fixnum-vector (n)
  (make-array n :element-type 'fixnum))

(defun acquire-lz77-scratch (token-size)
  "Get a LZ77-SCRATCH whose token arrays hold at least TOKEN-SIZE entries.
The hash heads and chain links are cleared to the empty-bucket sentinel
(#xFFFFFFFF), so every chain terminates inside the current input and
compression is fully deterministic across calls."
  (let ((s (with-scratch-lock (pop *scratch-pool*))))
    (unless s
      (setf s (%make-lzs
               :head (make-u32-vector +hash-size+)
               :prev (make-u32-vector +window-size+)
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
    (fill (lzs-head s) #xFFFFFFFF)
    (fill (lzs-prev s) #xFFFFFFFF)
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
  `(let* ((le (length-code+extra ,len))
          (de (dist-code+extra ,d))
          (code (logand le #xFF))
          (dcode (logand de #xFF)))
     (declare (type (unsigned-byte 32) le de)
              (type fixnum code dcode))
     (setf (aref sym nsym) (+ 257 code)
           (aref dist nsym) dcode
           (aref el nsym) ,len
           (aref ed nsym) ,d)
     (incf (aref lit-freq (+ 257 code)))
     (incf (aref dist-freq dcode))
     (incf extra-bits (+ (ash le -8) (ash de -8)))
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
                             +hash-mask+)))
              (declare (type fixnum h))
              ;; link Q after the current chain head, like INSERT-STRING
              (setf (aref prev (logand q +window-mask+)) (aref head h)
                    (aref head h) q)))))

(defmacro %lazy-drain-short ()
  "No room to search: flush any pending match, emit the rest as literals.
Leaves POS at END.  Uses the lazy loop's locals (see %LZ77-SEARCH-LAZY)."
  `(progn
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
       (incf pos))))

(defmacro %lazy-search-step ()
  "Insert POS, search, and resolve the new match against the pending one.
Uses the lazy loop's locals (see %LZ77-SEARCH-LAZY)."
  `(let ((cand (insert-string input pos head prev)))
     (declare (type fixnum cand))
     (let ((mlen 0) (mdist 0))
       (declare (type fixnum mlen mdist))
       (when (and (/= cand #xFFFFFFFF)
                  (or (not have-pending)
                      (< pending-len max-lazy)))
         (multiple-value-bind (len d)
             (longest-match input pos end cand head prev
                            max-chain nice good
                            (if have-pending pending-len (1- +min-match+))
                            isap)
           (setf mlen len mdist d)))
       (cond ((and have-pending
                   (>= pending-len +min-match+)
                   (<= mlen pending-len))
              (%emit-match pending-len pending-dist)
              (%insert-match-interior pending-pos pending-len (+ pending-pos 2))
              (setf pos (+ pending-pos pending-len)
                    have-pending nil))
             (have-pending
              (%emit-literal (1- pos))
              (incf pos)
              (setf pending-len mlen
                    pending-dist mdist
                    pending-pos (1- pos)))
             (t (setf have-pending t
                      pending-len mlen
                      pending-dist mdist
                      pending-pos pos)
                (incf pos))))))

(defun %lz77-search-lazy (input start end nice good max-chain max-lazy
                           sym dist el ed head prev lit-freq dist-freq)
  "Lazy matching (zlib deflate_slow): defer each match one position and
adopt it only if no longer match starts on the next byte."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed)
           (type (simple-array (unsigned-byte 32) (*)) head prev)
           (type (simple-array fixnum (*)) lit-freq dist-freq)
           (type fixnum start end nice good max-chain max-lazy))
  (let ((nsym 0) (extra-bits 0) (pos start)
        (have-pending nil) (pending-len 0) (pending-dist 0) (pending-pos 0))
    (declare (type fixnum nsym extra-bits pos pending-len pending-dist pending-pos))
    ;; Pin the input for the walk so the u16 best-pair check loads words
    ;; with one SAP taken once (output growth never moves INPUT).
    (with-pinned-input (isap input)
      (loop while (< pos end) do
        (if (< (- end pos) +min-match+)
            (%lazy-drain-short)
            (%lazy-search-step))))
    (when have-pending
      (if (>= pending-len +min-match+)
          (%emit-match pending-len pending-dist)
          (%emit-literal pending-pos)))
    (values nsym extra-bits)))

(defun %lz77-search-greedy (input start end nice good max-chain max-lazy
                             sym dist el ed head prev lit-freq dist-freq)
  "Greedy matching (zlib deflate_fast) for levels 1-3."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed)
           (type (simple-array (unsigned-byte 32) (*)) head prev)
           (type (simple-array fixnum (*)) lit-freq dist-freq)
           (type fixnum start end nice good max-chain max-lazy))
  (let ((nsym 0) (extra-bits 0) (pos start))
    (declare (type fixnum nsym extra-bits pos))
    (with-pinned-input (isap input)
      (loop while (< pos end) do
        (if (< (- end pos) +min-match+)
            (progn
              (%emit-literal pos)
              (incf pos))
            (let ((cand (insert-string input pos head prev)))
              (declare (type fixnum cand))
              (if (= cand #xFFFFFFFF)
                  (progn
                    (%emit-literal pos)
                    (incf pos))
                  (multiple-value-bind (len d)
                      (longest-match input pos end cand head prev
                                     max-chain nice good (1- +min-match+)
                                     isap)
                    (if (>= len +min-match+)
                        (progn
                          (%emit-match len d)
                          (when (<= len max-lazy)
                            (%insert-match-interior pos len (1+ pos)))
                          (incf pos len))
                        (progn
                          (%emit-literal pos)
                          (incf pos)))))))))
    (values nsym extra-bits)))

(defun %lz77-search (input start end nice good max-chain max-lazy lazy-p
                     sym dist el ed head prev lit-freq dist-freq)
  "LZ77 tokenization core; see RUN-LZ77.  Returns (VALUES NSYM EXTRA-BITS).
Dispatches to the lazy (levels 4-9) or greedy (levels 1-3) worker."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed)
           (type (simple-array (unsigned-byte 32) (*)) head prev)
           (type (simple-array fixnum (*)) lit-freq dist-freq)
           (type fixnum start end nice good max-chain max-lazy))
  (if lazy-p
      (%lz77-search-lazy input start end nice good max-chain max-lazy
                         sym dist el ed head prev lit-freq dist-freq)
      (%lz77-search-greedy input start end nice good max-chain max-lazy
                           sym dist el ed head prev lit-freq dist-freq)))

(defun run-lz77 (input start end level sym dist el ed lit-freq dist-freq
                 &optional (head (make-array +hash-size+
                                             :element-type '(unsigned-byte 32)
                                             :initial-element #xFFFFFFFF))
                           (prev (make-array +window-size+
                                             :element-type '(unsigned-byte 32)
                                             :initial-element #xFFFFFFFF)))
  "Run LZ77 over INPUT[START,END), filling SYM/DIST/EL/ED (sized to the
input length) and the symbol frequency vectors.  Returns (VALUES NSYM
EXTRA-BITS) where EXTRA-BITS is the total number of length/distance extra
bits across all matches.  Levels 1-3 use greedy matching (deflate_fast),
levels 4-9 use lazy matching (deflate_slow) which defers each match one
position and only adopts it if no longer match starts on the next byte."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed)
           (type (simple-array (unsigned-byte 32) (*)) head prev)
           (type (simple-array fixnum (*)) lit-freq dist-freq)
           (type fixnum start end level))
  (multiple-value-bind (nsym extra-bits)
      (%lz77-search input start end
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
           (* (+ 3 5 32) (1- (ceiling n +max-stored-block+)))))))

(defun stored-block-octets (n)
  (declare (type fixnum n))
  (let ((pad (mod (- 8 3) 8)))
    (ceiling (if (<= n +max-stored-block+)
                 (+ 3 pad 32 (* 8 n))
                 (+ (* 8 n)
                    (+ 3 pad 32)
                    (* (+ 3 5 32) (1- (ceiling n +max-stored-block+)))))
             8)))

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
    (write-bits writer (logand (lognot len) #xFFFF) 16)
    ;; after the 3 header bits (padded to a byte) and the two length words
    ;; the accumulator is byte-aligned again, so the payload can be copied
    ;; straight into the output buffer instead of going through WRITE-BITS
    (let ((buf (bw-buffer writer))
          (size (bw-size writer))
          (pos (bw-pos writer)))
      (declare (type fixnum size pos))
      (when (> (+ pos len) size)
        (let ((bigger (make-octet-buffer (* 4 (+ pos len)))))
          (replace bigger buf :end2 pos)
          (setf buf bigger
                size (length bigger))
          (setf (bw-buffer writer) buf
                (bw-size writer) size)))
      (replace buf input :start1 pos :start2 start :end1 (+ pos len) :end2 end)
      (setf (bw-pos writer) (+ pos len)))))

(declaim (inline %emit-coded-token))

(defun %emit-coded-token (writer s dd el-i ed-i lit-codes lit-lengths
                          dist-codes dist-lengths)
  "Write token S (literal or length code) with distance code DD: the literal
code, then for matches the length extra, distance code and distance extra."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum s dd el-i ed-i)
           (type (simple-array fixnum (*)) lit-codes lit-lengths
                 dist-codes dist-lengths))
  (write-bits writer (aref lit-codes s) (aref lit-lengths s))
  (when (> s 256)
    (let* ((be (length-base+extra (- s 257)))
           (n (ash be -16)))
      (declare (type (unsigned-byte 32) be) (type fixnum n))
      (when (plusp n)
        (write-bits writer (- el-i (logand be #xFFFF)) n)))
    (let* ((bde (dist-base+extra dd))
           (dn (ash bde -16)))
      (declare (type fixnum dn) (type (unsigned-byte 32) bde))
      (write-bits writer (aref dist-codes dd) (aref dist-lengths dd))
      (when (plusp dn)
        (write-bits writer (- ed-i (logand bde #xFFFF)) dn)))))

(defun emit-fixed-block (writer sym dist el ed nsym bfinal)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed)
           (type fixnum nsym))
  (ensure-static-trees)
  (write-bits writer (if bfinal 1 0) 1)
  (write-bits writer 1 2)
  (loop for i below nsym do
    (%emit-coded-token writer (aref sym i) (aref dist i)
                       (aref el i) (aref ed i)
                       +static-lit-codes+ +static-lit-lengths+
                       +static-dist-codes+ +static-dist-lengths+)))

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

(declaim (inline %bl-run-limits %bl-push-verbatim %bl-push-repeat
                 %bl-push-zeros))

(defun %bl-run-limits (curlen nextlen)
  "Count bounds for the run ending here: (VALUES MAX-COUNT MIN-COUNT)."
  (declare (type fixnum curlen nextlen))
  (cond ((zerop nextlen) (values 138 3))
        ((= curlen nextlen) (values 6 3))
        (t (values 7 4))))

(defun %bl-push-verbatim (bl-sym bl-extra bl-freq nbl curlen count)
  "Append COUNT literal code-length symbols; returns the new NBL."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (*)) bl-sym bl-extra bl-freq)
           (type fixnum nbl curlen count))
  (dotimes (k count nbl)
    (declare (ignore k))
    (setf (aref bl-sym nbl) curlen
          (aref bl-extra nbl) 0)
    (incf (aref bl-freq curlen))
    (incf nbl)))

(defun %bl-push-repeat (bl-sym bl-extra bl-freq nbl curlen count prevlen)
  "Append a code-16 repeat of the previous length; returns (VALUES NBL 2)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (*)) bl-sym bl-extra bl-freq)
           (type fixnum nbl curlen count prevlen))
  (when (/= curlen prevlen)
    (setf (aref bl-sym nbl) curlen
          (aref bl-extra nbl) 0)
    (incf (aref bl-freq curlen))
    (incf nbl)
    (decf count))
  (setf (aref bl-sym nbl) 16
        (aref bl-extra nbl) (- count 3))
  (incf (aref bl-freq 16))
  (values (1+ nbl) 2))

(defun %bl-push-zeros (bl-sym bl-extra bl-freq nbl count)
  "Append a code-17/18 zero run; returns (VALUES NBL EXTRA-BITS)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (*)) bl-sym bl-extra bl-freq)
           (type fixnum nbl count))
  (if (<= count 10)
      (progn
        (setf (aref bl-sym nbl) 17
              (aref bl-extra nbl) (- count 3))
        (incf (aref bl-freq 17))
        (values (1+ nbl) 3))
      (progn
        (setf (aref bl-sym nbl) 18
              (aref bl-extra nbl) (- count 11))
        (incf (aref bl-freq 18))
        (values (1+ nbl) 7))))

(defun scan-code-lengths (lengths n bl-sym bl-extra bl-freq nbl)
  "RLE-encode LENGTHS[0..N) into code-length symbols (RFC 1951 3.2.7),
appending to BL-SYM/BL-EXTRA starting at NBL and counting frequencies into
BL-FREQ.  Returns (VALUES NBL EXTRA-BITS)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (*)) lengths bl-sym bl-extra bl-freq)
           (type fixnum n nbl))
  (let ((count 0) (prevlen -1) (extra-bits 0))
    (declare (type fixnum count prevlen extra-bits))
    (let ((nextlen (if (plusp n) (aref lengths 0) -1)))
      (declare (type fixnum nextlen))
      (multiple-value-bind (max-count min-count)
          (if (zerop nextlen) (values 138 3) (values 7 4))
        (declare (type fixnum max-count min-count))
        (iterate:iterate
          (iterate:for idx from 0 below n)
          (iterate:for curlen = nextlen)
          (setf nextlen (if (< idx (1- n)) (aref lengths (1+ idx)) -1))
          (incf count)
          (unless (and (< count max-count) (= curlen nextlen))
            (cond ((< count min-count)
                   (setf nbl (%bl-push-verbatim bl-sym bl-extra bl-freq
                                                nbl curlen count)))
                  ((not (zerop curlen))
                   (multiple-value-bind (nn eb)
                       (%bl-push-repeat bl-sym bl-extra bl-freq
                                        nbl curlen count prevlen)
                     (setf nbl nn)
                     (incf extra-bits eb)))
                  (t (multiple-value-bind (nn eb)
                         (%bl-push-zeros bl-sym bl-extra bl-freq nbl count)
                       (setf nbl nn)
                       (incf extra-bits eb))))
            (setf count 0 prevlen curlen)
            (multiple-value-bind (mx mn) (%bl-run-limits curlen nextlen)
              (setf max-count mx min-count mn)))))
      (values nbl extra-bits))))

(declaim (inline %emit-bl-header %emit-bl-symbols))

(defun %emit-bl-header (writer bfinal hlit hdist hclen)
  "Write a dynamic block's 3-bit type plus HLIT/HDIST/HCLEN counts."
  (declare (type fixnum hlit hdist hclen))
  (write-bits writer (if bfinal 1 0) 1)
  (write-bits writer 2 2)
  (write-bits writer (- hlit 257) 5)
  (write-bits writer (- hdist 1) 5)
  (write-bits writer (- hclen 4) 4))

(defun %emit-bl-symbols (writer bl-sym bl-extra bl-codes bl-lengths nbl hclen)
  "Write the code-length order table then the NBL code-length symbols."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (*)) bl-sym bl-extra bl-codes bl-lengths)
           (type fixnum nbl hclen))
  (loop for i below hclen do
    (write-bits writer (aref bl-lengths (aref +code-length-order+ i)) 3))
  (loop for i below nbl do
    (let ((s (aref bl-sym i)))
      (write-bits writer (aref bl-codes s) (aref bl-lengths s))
      (case s
        (16 (write-bits writer (aref bl-extra i) 2))
        (17 (write-bits writer (aref bl-extra i) 3))
        (18 (write-bits writer (aref bl-extra i) 7))
        (otherwise nil)))))

(defun emit-dynamic-block (writer sym dist el ed nsym bfinal
                            lit-codes lit-lengths dist-codes dist-lengths
                            bl-sym bl-extra bl-codes bl-lengths nbl
                            hlit hdist hclen)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 16) (*)) sym dist el ed)
           (type (simple-array fixnum (*)) lit-codes lit-lengths dist-codes
                              dist-lengths bl-sym bl-extra bl-codes bl-lengths)
           (type fixnum nsym nbl hlit hdist hclen))
  (%emit-bl-header writer bfinal hlit hdist hclen)
  (%emit-bl-symbols writer bl-sym bl-extra bl-codes bl-lengths nbl hclen)
  (loop for i below nsym do
    (%emit-coded-token writer (aref sym i) (aref dist i)
                       (aref el i) (aref ed i)
                       lit-codes lit-lengths dist-codes dist-lengths)))

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

(defun %emit-tiny-choice (writer input start end n nsym extra-bits scratch)
  "Stored-vs-fixed choice for inputs of at most 1024 bytes (dynamic trees
rarely pay for themselves at that size)."
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end n nsym extra-bits))
  (let ((stored-size (stored-block-bits writer n))
        (fixed-size (+ 3 (data-bits (lzs-sym scratch) (lzs-distc scratch)
                                    nsym +static-lit-lengths+
                                    +static-dist-lengths+ extra-bits))))
    (declare (type fixnum stored-size fixed-size))
    (ensure-static-trees)
    (if (<= stored-size fixed-size)
        (emit-stored-blocks writer input start end)
        (emit-fixed-block writer (lzs-sym scratch) (lzs-distc scratch)
                          (lzs-el scratch) (lzs-ed scratch) nsym t))))

(defun %span-of-used (lengths hi lo floor)
  "One plus the highest used index in LENGTHS[LO..HI], at least FLOOR."
  (declare (type (simple-array fixnum (*)) lengths)
           (type fixnum hi lo floor))
  (max floor (1+ (loop for i from hi downto lo
                       when (plusp (aref lengths i)) return i))))

(defun %plan-bl-trees (scratch lit-lengths hlit dist-lengths hdist)
  "Scan lit/dist lengths and build the code-length tree.  Returns (VALUES
NBL BL-EXTRA HCLEN BL-CODE-BITS)."
  (declare (type fixnum hlit hdist))
  (multiple-value-bind (nbl1 bl-extra1)
      (scan-code-lengths lit-lengths hlit
                         (lzs-bl-sym scratch) (lzs-bl-extra scratch)
                         (lzs-bl-freq scratch) 0)
    (declare (ignore bl-extra1))
    (multiple-value-bind (nbl bl-extra)
        (scan-code-lengths dist-lengths hdist
                           (lzs-bl-sym scratch) (lzs-bl-extra scratch)
                           (lzs-bl-freq scratch) nbl1)
      (multiple-value-bind (bl-lengths bl-codes bl-max)
          (build-huffman-codes (lzs-bl-freq scratch) 19 7
                               (lzs-work scratch)
                               (lzs-bl-lengths scratch) (lzs-bl-codes scratch))
        (declare (ignore bl-codes bl-max))
        (let ((hclen 4) (bl-code-bits 0))
          (declare (type fixnum hclen bl-code-bits))
          (loop for rank from 18 downto 3
                when (plusp (aref bl-lengths (aref +code-length-order+ rank)))
                do (setf hclen (1+ rank)) (return))
          (loop for i below nbl do
            (incf bl-code-bits (aref bl-lengths (aref (lzs-bl-sym scratch) i))))
          (values nbl bl-extra hclen bl-code-bits))))))

(defun %emit-cheapest (writer input start end scratch nsym
                       opt-size fixed-size stored-size
                       lit-codes lit-lengths dist-codes dist-lengths
                       nbl hlit hdist hclen)
  "Emit the cheapest of stored / fixed / dynamic for a large block."
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end nsym opt-size fixed-size stored-size
                   nbl hlit hdist hclen))
  (cond ((<= stored-size (min opt-size fixed-size))
         (emit-stored-blocks writer input start end))
        ((<= fixed-size opt-size)
         (emit-fixed-block writer (lzs-sym scratch) (lzs-distc scratch)
                           (lzs-el scratch) (lzs-ed scratch) nsym t))
        (t (emit-dynamic-block writer (lzs-sym scratch) (lzs-distc scratch)
                               (lzs-el scratch) (lzs-ed scratch) nsym t
                               lit-codes lit-lengths dist-codes dist-lengths
                               (lzs-bl-sym scratch) (lzs-bl-extra scratch)
                               (lzs-bl-codes scratch) (lzs-bl-lengths scratch)
                               nbl hlit hdist hclen))))

(defun %emit-optimized-block (writer input start end n nsym extra-bits scratch)
  "Full dynamic-tree planning plus cheapest-encoding emit for large inputs."
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end n nsym extra-bits))
  (multiple-value-bind (lit-lengths lit-codes lit-max)
      (build-huffman-codes (lzs-lit-freq scratch) 286 15
                           (lzs-work scratch)
                           (lzs-lit-lengths scratch) (lzs-lit-codes scratch))
    (declare (ignore lit-max))
    (multiple-value-bind (dist-lengths dist-codes dist-max)
        (build-huffman-codes (lzs-dist-freq scratch) 30 15
                             (lzs-work scratch)
                             (lzs-dist-lengths scratch) (lzs-dist-codes scratch))
      (declare (ignore dist-max))
      (let ((hlit (%span-of-used lit-lengths 285 256 257))
            (hdist (%span-of-used dist-lengths 29 0 1)))
        (declare (type fixnum hlit hdist))
        (multiple-value-bind (nbl bl-extra hclen bl-code-bits)
            (%plan-bl-trees scratch lit-lengths hlit dist-lengths hdist)
          (declare (type fixnum nbl bl-extra hclen bl-code-bits))
          (ensure-static-trees)
          (multiple-value-bind (dyn-bits fixed-bits)
              (data-bits/dynamic-and-fixed
               (lzs-sym scratch) (lzs-distc scratch) nsym
               lit-lengths +static-lit-lengths+
               dist-lengths +static-dist-lengths+ extra-bits)
            (declare (type fixnum dyn-bits fixed-bits))
            (%emit-cheapest writer input start end scratch nsym
                            (+ 3 14 (* 3 hclen) bl-extra bl-code-bits dyn-bits)
                            (+ 3 fixed-bits) (stored-block-bits writer n)
                            lit-codes lit-lengths dist-codes dist-lengths
                            nbl hlit hdist hclen)))))))

(defun deflate-into-writer (input start end writer level &optional scratch)
  "Compress INPUT[START,END) into WRITER as one DEFLATE stream.  Level 0
emits stored blocks; higher levels pick the cheapest block encoding."
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type bit-writer writer)
           (type fixnum start end level)
           (type (or null lz77-scratch) scratch))
  (let ((n (- end start)))
    (declare (type fixnum n))
    (if (zerop level)
        (emit-stored-blocks writer input start end)
        (let ((owned (null scratch)))
          (unless scratch
            (setf scratch (acquire-lz77-scratch (1+ n))))
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
                   (declare (type fixnum nsym extra-bits))
                   (if (<= n 1024)
                       (%emit-tiny-choice writer input start end
                                          n nsym extra-bits scratch)
                       (%emit-optimized-block writer input start end
                                              n nsym extra-bits scratch))))
            (when owned
              (release-lz77-scratch scratch)))))))

(defun deflate-raw (input &optional (start 0) (end (length input)) (level 6))
  "Compress INPUT[START,END) with raw DEFLATE (no zlib/gzip header) at LEVEL.
Returns a fresh octet vector."
  (check-compression-level level)
  (unless (typep input '(simple-array (unsigned-byte 8) (*)))
    (error 'newzlib-parameter-error :detail "input must be an (unsigned-byte 8) vector"))
  (let ((n (- end start)))
    (declare (type fixnum n))
    (if (zerop level)
        (let* ((out (make-octet-buffer (stored-block-octets n)))
               (writer (make-bit-writer-for-buffer out)))
          (emit-stored-blocks writer input start end)
          (flush-bits writer)
          out)
        (let* ((bound (+ n (ash n -3) 256))
               (scratch (acquire-lz77-scratch (1+ n)))
               (writer (reset-bit-writer (lzs-writer scratch) bound)))
          (unwind-protect
               (progn
                 (deflate-into-writer input start end writer level scratch)
                 (writer-bytes writer))
            (release-lz77-scratch scratch))))))

(defun check-compression-buffer (buffer required)
  (unless (typep buffer '(simple-array (unsigned-byte 8) (*)))
    (error 'newzlib-parameter-error
           :detail "output must be a simple (unsigned-byte 8) vector"))
  (unless (>= (length buffer) required)
    (error 'newzlib-parameter-error
           :detail (format nil "output buffer has ~D octets; ~D required"
                           (length buffer) required)))
  buffer)

(defun deflate-raw-into (output input level)
  (check-compression-level level)
  (unless (typep input '(simple-array (unsigned-byte 8) (*)))
    (error 'newzlib-parameter-error
           :detail "input must be an (unsigned-byte 8) vector"))
  (when (eq output input)
    (error 'newzlib-parameter-error
           :detail "output buffer must not alias input"))
  (let ((n (length input)))
    (check-compression-buffer
     output
     (if (zerop level)
         (stored-block-octets n)
         (+ n (ash n -3) 256)))
    (let ((writer (make-bit-writer-for-buffer output)))
      (if (zerop level)
          (progn
            (deflate-into-writer input 0 n writer level)
            (flush-bits writer))
          (let ((scratch (acquire-lz77-scratch (1+ n))))
            (unwind-protect
                 (progn
                   (deflate-into-writer input 0 n writer level scratch)
                   (flush-bits writer))
              (release-lz77-scratch scratch))))
      (bw-pos writer))))
