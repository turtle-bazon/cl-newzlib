(in-package #:cl-newzlib)

;;; Vectorized Adler-32 via AVX2 (SBCL/x86-64 with sb-simd only).
;;;
;;; Structure follows zlib-ng's AVX2 Adler (deferred multiply: per-32-byte
;;; block accumulators plus a shifted s1 history, NMAX-style chunking),
;;; with own code throughout.  s1/s2 live in s32.8 packs (all partials fit
;;; signed 32 bits at our 2048-byte chunk size); only the horizontal sums
;;; and the mod folds touch scalars.
;;;
;;; Enabled only after a load-time self-test passes on the running CPU
;;; (AVX2 gating plus randomized differential checks against the scalar
;;; engine); anything else leaves *simd-adler-impl* NIL and the library
;;; silently keeps the scalar loop.  Other Lisps load this file inertly.

#+(and sbcl x86-64 newzlib-avx2)
(progn
  (defvar *adler-w32* nil)   ; s8 weights [32..1], one 32-byte vector
  (defvar *adler-ones* nil)  ; s16 ones
  (defvar *adler-zeros* nil) ; u8 zeros (SAD baseline)
  ;; A runtime zero source.  If MAKE-S32.8 were called with literal zeros,
  ;; SBCL could constant-fold it while compiling and execute AVX2 on an
  ;; AVX2-less host (the reported SB-SIMD-AVX2::S32.8 SIGILL).
  (defvar *simd-zero* 0)

  ;; Weight tables are plain octet vectors, built once at load.
  (let ((w32 (make-array 32 :element-type '(signed-byte 8)))
        (ones (make-array 16 :element-type '(signed-byte 16) :initial-element 1))
        (z (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i 32)
      (setf (aref w32 i) (- 32 i)))
    (setf *adler-w32* w32 *adler-ones* ones *adler-zeros* z))

  (declaim (inline adler-vec-chunk))
  (defun adler-vec-chunk (s1 s2 octets start end)
    "Vector-fold OCTETS[START,END) -- a multiple of 32 bytes no larger than
2048 -- into the u32 scalars S1/S2 (already folded).  Returns (VALUES NS1
NS2) unfolded; the caller folds both mod 65521 afterwards.  Lanes stay
below 2^31 throughout (signed view is safe), and s1/s2 stay below 2^32."
    (declare (type (unsigned-byte 32) s1 s2)
             (type (simple-array (unsigned-byte 8) (*)) octets)
             (type fixnum start end)
             ;; SBCL can constant-fold a constructor call with all-constant
             ;; arguments and execute it at compile time.  The zero pack is
             ;; sourced from a runtime variable below; keep this call out of
             ;; line as a second line of defense.
             (notinline sb-simd-avx2:make-s32.8)
             (optimize (speed 3) (safety 0) (debug 0)))
    ;; On a build host without AVX2, force all AVX2 operations out of line so
    ;; compilation cannot execute any of them.  On an AVX2 build host the
    ;; fast VOP versions stay available.
    #-newzlib-avx2-build
    (declare (notinline sb-simd-avx2:s32.8+
                        sb-simd-avx2:s32.8-shiftl
                        sb-simd-avx2:s32.8-values
                        sb-simd-avx2:u8.32-sad
                        sb-simd-avx2:s16.16-madd
                        sb-simd-avx2:s16.16-maddubs
                        sb-simd-avx:s32.8!
                        sb-simd-avx:s8.32-aref
                        sb-simd-avx:s16.16-aref
                        sb-simd-avx:u8.32-aref))
    ;; Zero-extended starts (lane 0 only): the SAD high halves stay zero,
    ;; so the final full horizontal sum counts the initial sums once.
    (let ((vs1 (sb-simd-avx2:make-s32.8 s1 0 0 0 0 0 0 0))
          (vs2 (sb-simd-avx2:make-s32.8 s2 0 0 0 0 0 0 0))
          (vsz (sb-simd-avx2:make-s32.8
                *simd-zero* *simd-zero* *simd-zero* *simd-zero*
                *simd-zero* *simd-zero* *simd-zero* *simd-zero*)))
      (declare (type sb-simd-avx2:s32.8 vs1 vs2 vsz))
      (let ((vs1-0 vs1)
            (vs3 vsz)
            (w32 (sb-simd-avx2:s8.32-aref *adler-w32* 0))
            (ones (sb-simd-avx2:s16.16-aref *adler-ones* 0))
            (z (sb-simd-avx2:u8.32-aref *adler-zeros* 0))
            (pos start))
        (declare (type sb-simd-avx2:s32.8 vs1-0 vs3)
                 (type sb-simd-avx2:s8.32 w32)
                 (type sb-simd-avx2:s16.16 ones)
                 (type sb-simd-avx2:u8.32 z)
                 (type fixnum pos))
        (loop while (>= (- end pos) 32) do
          (let ((vbuf (sb-simd-avx2:u8.32-aref octets pos)))
            (setf vs1 (sb-simd-avx2:s32.8+
                       vs1 (sb-simd-avx2:s32.8!
                            (sb-simd-avx2:u8.32-sad vbuf z)))
                  vs3 (sb-simd-avx2:s32.8+ vs3 vs1-0)
                  vs2 (sb-simd-avx2:s32.8+
                       vs2 (sb-simd-avx2:s32.8!
                            (sb-simd-avx2:s16.16-madd
                             (sb-simd-avx2:s16.16-maddubs vbuf w32)
                             ones)))
                  vs1-0 vs1
                  pos (+ pos 32))))
        ;; deferred multiply: each 32-byte block owed 32*s1-at-its-start
        (setf vs3 (sb-simd-avx2:s32.8-shiftl vs3 5)
              vs2 (sb-simd-avx2:s32.8+ vs2 vs3))
        (multiple-value-bind (a b c d e f g h) (sb-simd-avx2:s32.8-values vs1)
          (multiple-value-bind (i j k l m n o p) (sb-simd-avx2:s32.8-values vs2)
            (values (+ a b c d e f g h) (+ i j k l m n o p)))))))

  (defun adler32-vector (octets start end initial-adler)
    "Vector Adler-32 over OCTETS[START,END) given INITIAL-ADLER.  Processes
2048-byte vector chunks (32-byte-multiple tail chunk included), then hands
the <32-byte remainder to the scalar engine with the unfolded sums."
    (declare (type (simple-array (unsigned-byte 8) (*)) octets)
             (type fixnum start end)
             (type (unsigned-byte 32) initial-adler)
             (optimize (speed 3) (safety 0) (debug 0)))
    (let ((s1 (ldb (byte 16 0) initial-adler))
          (s2 (ldb (byte 16 16) initial-adler))
          (pos start))
      (declare (type (unsigned-byte 32) s1 s2)
               (type fixnum pos))
      (loop while (>= (- end pos) 2048) do
        (multiple-value-bind (ns1 ns2)
            (adler-vec-chunk s1 s2 octets pos (+ pos 2048))
          (setf s1 (mod ns1 +adler-mod+)
                s2 (mod ns2 +adler-mod+)
                pos (+ pos 2048))))
      (let ((vlen (- (- end pos) (mod (- end pos) 32))))
        (when (>= vlen 32)
          (multiple-value-bind (ns1 ns2)
              (adler-vec-chunk s1 s2 octets pos (+ pos vlen))
            (setf s1 (mod ns1 +adler-mod+)
                  s2 (mod ns2 +adler-mod+)
                  pos (+ pos vlen)))))
      (%adler32 octets pos end s1 s2)))

  ;;; ------------------------------------------------------------------
  ;;; Availability probe + load-time self-test.
  ;;; ------------------------------------------------------------------
  (defun simd-adler-cpu-available-p ()
    "True when AVX2 is implemented and enabled by the OS."
    (avx2-usable-p))

  (defun simd-adler-self-test-p ()
    "Randomized differential check of the vector path against the scalar
engine: unaligned starts, sizes across chunk/block/tail boundaries, folded
and unfolded initial sums."
    (let ((data (make-array 5000 :element-type '(unsigned-byte 8)))
          (s 7777777))
      (flet ((rnd (bound)
               (setf s (logand (+ (* s 6364136223846793005) 1442695040888963407)
                               #xFFFFFFFFFFFFFFFF))
               (mod s bound)))
        (dotimes (i 5000)
          (setf (aref data i) (rnd 256)))
        (dotimes (i 40 t)
          (let* ((len (+ 32 (rnd 4900)))
                 (start (rnd 18))
                 (end (+ start len))
                 (buf (make-array (+ end 16) :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
            (dotimes (k len)
              (setf (aref buf (+ start k)) (aref data (mod (+ (* i 31) k) 5000))))
            (let ((init (rnd #x100000000)))
              (unless (= (adler32-vector buf start end (ldb (byte 32 0) init))
                         (%adler32 buf start end
                                   (ldb (byte 16 0) init)
                                   (ldb (byte 16 16) init)))
                (return-from simd-adler-self-test-p nil))))))))

  (defvar *simd-adler-self-test-passed* nil
    "True once the vector path has passed on a capable CPU.")

  (defun update-simd-adler-impl ()
    "Select the Adler implementation for the current CPU.  Runs the self-test
lazily when the CPU gains AVX2 (e.g. an image built on an AVX-only machine
restarted on an AVX2 host), and clears the implementation when it does not."
    (setf *simd-adler-impl*
          (when (and (simd-adler-cpu-available-p)
                     (or *simd-adler-self-test-passed*
                         (setf *simd-adler-self-test-passed*
                               (handler-case (simd-adler-self-test-p)
                                 (error () nil)))))
            #'adler32-vector)))

  (update-simd-adler-impl)
  #+sbcl (pushnew 'update-simd-adler-impl sb-ext:*init-hooks*))
