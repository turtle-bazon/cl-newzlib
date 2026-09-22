(in-package #:cl-newzlib)

;;; Hardware CRC-32 via PCLMULQDQ (SBCL/x86-64, little-endian only).
;;;
;;; Structure follows zlib-ng's parallel fold (fold four 128-bit lanes per
;;; 64 bytes, combine, two-stage Barrett reduction); constants are theirs.
;;; Everything else here is original: the PCLMULQDQ operations go through
;;; house VOPs (sb-assem already knows the instruction, sb-simd never
;;; wrapped it), loads through sb-simd, lane plumbing in portable Lisp.
;;;
;;; Design points:
;;; - The bulk prefix (a multiple of 64 bytes, plus a 16-byte preamble when
;;;   the initial CRC is nonzero) folds through hardware; any remainder and
;;;   all short inputs keep using the slicing updater.  crc32-update
;;;   dispatches here through *pclmul-crc-impl* and slices the tail, so the
;;;   hardware path never needs partial-block logic.
;;; - Enabled only after a load-time self-test (known vectors plus
;;;   randomized differential checks against the slicer, including
;;;   unaligned starts and nonzero initial CRCs) passes on the running CPU.
;;;   Anything else -- no PCLMULQDQ flag, VOP trouble, self-test mismatch --
;;;   leaves *pclmul-crc-impl* NIL and the library silently keeps slicing.
;;;   (The whole file is inert without SBCL/x86-64 little-endian plus
;;;   sb-simd: the x86-64 gate matters because the VOP emitters and sb-simd's
;;;   CPUID helper exist only there.)

#+(and sbcl x86-64 cl-newzlib-le newzlib-simd)
(progn
  ;;; ------------------------------------------------------------------
  ;;; PCLMULQDQ VOPs, one per imm8 used (monomorphic keeps each generator
  ;;; trivial).  imm8 bit 0 selects the destination half, bit 4 the source
  ;;; half; verified against C intrinsics on all four combinations.
  ;;;
  ;;; The eval-when is load-bearing, not style: VOP templates must be
  ;;; REGISTERED when later forms in this file compile, and
  ;;; COMPILE-FILE does not evaluate top-level define-vop forms (unlike
  ;;; LOAD).  Without this, same-file call sites silently fall back to
  ;;; generic calls.  :overwrite-fndb-silently keeps interactive
  ;;; recompile+reload working.
  ;;; ------------------------------------------------------------------
  (eval-when (:compile-toplevel :load-toplevel :execute)
   (macrolet ((defpclmul (fun vop imm)
                `(progn
                   (sb-c:defknown ,fun
                       (sb-simd-sse2:u64.2 sb-simd-sse2:u64.2)
                       (values sb-simd-sse2:u64.2 &optional)
                       (sb-c::flushable)
                     :overwrite-fndb-silently t)
                   (defun ,fun (x y)
                     (declare (type sb-simd-sse2:u64.2 x y)
                              (optimize (speed 3) (safety 0)))
                     (error "PCLMULQDQ VOP did not apply for ~S" ',fun))
                   (sb-c:define-vop (,vop)
                     (:translate ,fun)
                     (:policy :fast-safe)
                     (:args (x :scs (sb-vm::int-sse-reg) :target r)
                            (y :scs (sb-vm::int-sse-reg)))
                     (:temporary (:sc sb-vm::int-sse-reg :from (:argument 0)) tmp)
                     (:results (r :scs (sb-vm::int-sse-reg)))
                     (:arg-types sb-vm::simd-pack-ub64 sb-vm::simd-pack-ub64)
                     (:result-types sb-vm::simd-pack-ub64)
                     (:generator 4
                       (cond ((sb-c:location= x r)
                              (sb-assem:inst pclmulqdq r y ,imm))
                             ((or (not (sb-c:tn-p y))
                                  (not (sb-c:location= y r)))
                              (sb-c:move r x)
                              (sb-assem:inst pclmulqdq r y ,imm))
                             (t
                              (sb-c:move tmp x)
                              (sb-assem:inst pclmulqdq tmp y ,imm)
                              (sb-c:move r tmp))))))))
    (defpclmul %pclmulqdq-00 pclmulqdq-00 #x00)
    (defpclmul %pclmulqdq-01 pclmulqdq-01 #x01)
    (defpclmul %pclmulqdq-10 pclmulqdq-10 #x10)))

  ;;; ------------------------------------------------------------------
  ;;; Fold constants (zlib-ng's, as (lo, hi) lane pairs).
  ;;; ------------------------------------------------------------------
  (defvar *pcl-fold4* (sb-simd-sse2:make-u64.2 #x00000001c6e41596 #x0000000154442bd4))
  (defvar *pcl-k12* (sb-simd-sse2:make-u64.2 #x00000000ccaa009e #x00000001751997d0))
  (defvar *pcl-barrett-k* (sb-simd-sse2:make-u64.2 #xb4e5b025f7011641 #x00000001db710640))
  (defvar *pcl-blend-mask* (sb-simd-sse2:make-u64.2 0 #xFFFFFFFF))
  (defvar *pcl-zero* (sb-simd-sse2:make-u64.2 0 0))

  (declaim (inline pcl-load128 pcl-xor3 pcl-fold-step))
  (defun pcl-load128 (octets i)
    "Load 16 bytes at byte offset I as a u64.2 pack (movdqu: any alignment)."
    (declare (type (simple-array (unsigned-byte 8) (*)) octets)
             (type fixnum i)
             (optimize (speed 3) (safety 0)))
    (sb-simd-sse2:u64.2! (sb-simd-sse2:u8.16-aref octets i)))

  (defun pcl-xor3 (a b c)
    (declare (type sb-simd-sse2:u64.2 a b c)
             (optimize (speed 3) (safety 0)))
    (sb-simd-sse2:u64.2-xor (sb-simd-sse2:u64.2-xor a b) c))

  (defun pcl-fold-step (s k)
    "One fold: xor(clmul(s.hi,k.lo), clmul(s.lo,k.hi)) as a pack."
    (declare (type sb-simd-sse2:u64.2 s k)
             (optimize (speed 3) (safety 0)))
    (sb-simd-sse2:u64.2-xor (%pclmulqdq-01 s k) (%pclmulqdq-10 s k)))

  ;;; ------------------------------------------------------------------
  ;;; Bulk core: external CRC in, (VALUES external-mid-crc pos) out.
  ;;; Consumes a 64-byte-multiple prefix (plus the 16-byte preamble when
  ;;; CRC is nonzero); the caller slices [POS, END).
  ;;; ------------------------------------------------------------------
  (defun crc32-update/pclmul (crc octets start end)
    (declare (type (unsigned-byte 32) crc)
             (type (simple-array (unsigned-byte 8) (*)) octets)
             (type fixnum start end)
             (optimize (speed 3) (safety 0) (debug 0)))
    (let ((s0 (sb-simd-sse2:make-u64.2 #x9db42487 0))
          (s1 *pcl-zero*) (s2 *pcl-zero*) (s3 *pcl-zero*)
          (pos start)
          (k *pcl-fold4*)
          ;; Bind the constant packs to declared locals: VOP translation
          ;; needs statically-known pack types, which globals (type T)
          ;; don't provide -- passing globals straight to the VOP calls
          ;; silently falls back to generic calls.
          (kc *pcl-k12*)
          (bk *pcl-barrett-k*)
          (bm *pcl-blend-mask*))
      (declare (type sb-simd-sse2:u64.2 s0 s1 s2 s3 k kc bk bm)
               (type fixnum pos))
      (unless (zerop crc)
        ;; preamble: fold old s0, rotate, xor first 16 bytes + crc into s3
        (let ((t0 (pcl-load128 octets pos))
              (folded (pcl-fold-step s0 k)))
          (setf s0 s1 s1 s2 s2 s3
                s3 folded
                s3 (pcl-xor3 s3 t0 (sb-simd-sse2:make-u64.2 crc 0))
                pos (+ pos 16))))
      (loop while (>= (- end pos) 64) do
        (let ((t0 (pcl-load128 octets pos))
              (t1 (pcl-load128 octets (+ pos 16)))
              (t2 (pcl-load128 octets (+ pos 32)))
              (t3 (pcl-load128 octets (+ pos 48))))
          (setf s0 (pcl-fold-step s0 k)
                s1 (pcl-fold-step s1 k)
                s2 (pcl-fold-step s2 k)
                s3 (pcl-fold-step s3 k)
                s0 (sb-simd-sse2:u64.2-xor s0 t0)
                s1 (sb-simd-sse2:u64.2-xor s1 t1)
                s2 (sb-simd-sse2:u64.2-xor s2 t2)
                s3 (sb-simd-sse2:u64.2-xor s3 t3)
                pos (+ pos 64))))
      ;; fold the four lanes into one (k12), sequentially: each step folds
      ;; the previous lane's state into the next
      (setf s1 (pcl-xor3 s1 (%pclmulqdq-01 s0 kc) (%pclmulqdq-10 s0 kc)))
      (setf s2 (pcl-xor3 s2 (%pclmulqdq-01 s1 kc) (%pclmulqdq-10 s1 kc)))
      (setf s3 (pcl-xor3 s3 (%pclmulqdq-01 s2 kc) (%pclmulqdq-10 s2 kc)))
      ;; two-stage Barrett reduction to 32 bits
      (let* ((tmp0 (%pclmulqdq-00 s3 bk))
             (tmp1 (%pclmulqdq-10 tmp0 bk))
             (tmp1m (sb-simd-sse2:u64.2-and tmp1 bm))
             (tmp0b (sb-simd-sse2:u64.2-xor tmp1m s3))
             (ra (%pclmulqdq-01 tmp0b bk))
             (rb (%pclmulqdq-10 ra bk)))
        (multiple-value-bind (l0 l1) (sb-simd-sse2:u64.2-values rb)
          (declare (ignore l0))
          (values (logxor (ldb (byte 32 0) l1) #xFFFFFFFF) pos)))))

  ;;; ------------------------------------------------------------------
  ;;; Availability probe + load-time self-test.
  ;;; ------------------------------------------------------------------
  (defun pclmul-cpu-available-p ()
    "True when CPUID leaf 1 reports PCLMULQDQ in ECX (bit 1), queried from
inside the implementation via sb-simd's CPUID wrapper -- no /proc or other
OS channel involved, so this works wherever SBCL/x86-64 runs."
    (handler-case
        (and (>= (sb-simd-internals::cpuid 0) 1)
             (logbitp 1 (nth-value 2 (sb-simd-internals::cpuid 1))))
      (error () nil)))

  (defun pclmul-self-test-p ()
    "Randomized differential check of the hardware path against the
slicing updater: unaligned starts, short and long lengths, zero and
nonzero initial CRCs."
    (let ((data (make-array 2048 :element-type '(unsigned-byte 8)))
          (s 1234567))
      (flet ((rnd (bound)
               (setf s (logand (+ (* s 6364136223846793005) 1442695040888963407)
                               #xFFFFFFFFFFFFFFFF))
               (mod s bound)))
        (dotimes (i 2048)
          (setf (aref data i) (rnd 256)))
        ;; known vector through the public entry (exercises flips too)
        (unless (= (crc32 (map '(vector (unsigned-byte 8))
                                #'char-code "123456789"))
                   #xCBF43926)
          (return-from pclmul-self-test-p nil))
        (dotimes (i 24 t)
          (let* ((len (+ 64 (rnd 1900)))
                 (start (rnd 18))
                 (end (+ start len))
                 (buf (make-array (+ end 16) :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
            (dotimes (k len)
              (setf (aref buf (+ start k)) (aref data (mod (+ (* i 31) k) 2048))))
            (let ((crc (rnd #x100000000)))
              ;; the oracle forces the slicing path by rebinding the impl
              ;; away (it is still NIL during this self-test, so this is
              ;; belt-and-braces against load-order surprises)
              (unless (= (crc32-update crc buf start end)
                         (let ((*pclmul-crc-impl* nil))
                           (crc32-update crc buf start end)))
                (return-from pclmul-self-test-p nil))))))))

  (when (and (pclmul-cpu-available-p)
             (handler-case (pclmul-self-test-p) (error () nil)))
    (setf *pclmul-crc-impl* #'crc32-update/pclmul)))
