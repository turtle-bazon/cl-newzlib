(in-package #:cl-newzlib)

;;; RFC 1952 gzip wrapper format.
;;;
;;;   +---+---+---+---+---+---+---+---+---+---+
;;;   |ID1|ID2|CM |FLG|     MTIME     |XFL|OS |
;;;   +---+---+---+---+---+---+---+---+---+---+
;;;   (optional: FEXTRA / FNAME / FCOMMENT / FHCRC)
;;;   |     DEFLATE DATA     |
;;;   |     CRC32      |  ISIZE    |
;;;
;;; Header fields we emit: ID1=0x1F ID2=0x8B CM=8, no optional fields,
;;; MTIME=0, XFL by level, OS=3 (Unix).  CRC32 and ISIZE are little-endian.

(defun gzip-xfl (level)
  (cond ((>= level 9) 2)
        ((= level 1) 4)
        (t 0)))

(defun gzip-header-octets (level)
  (make-array 10 :element-type '(unsigned-byte 8)
              :initial-contents (list #x1F #x8B 8 0
                                      0 0 0 0
                                      (gzip-xfl level) 3)))

(defun gzip-compress (input &optional (start 0) (end (length input))
                            (level +default-compression+))
  "Compress INPUT[START,END) into the RFC 1952 gzip format.  Returns a fresh
octet vector."
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end))
  (check-compression-level level)
  ;; deflate straight into the pooled writer buffer behind the ten header
  ;; bytes; one final copy produces the exact-size result with trailer
  (let* ((n (- end start))
         (scratch (acquire-lz77-scratch (1+ n)))
         (writer (reset-bit-writer (lzs-writer scratch) (+ n (ash n -3) 274))))
    (unwind-protect
         (progn
           (setf (bw-pos writer) 10)
           (let ((header (gzip-header-octets level))
                 (buf (bw-buffer writer)))
             (replace buf header :end2 10))
           (deflate-into-writer input start end writer level)
           (flush-bits writer)
           (let* ((pos (bw-pos writer))
                  (crc (crc32 input start end))
                  (size (ldb (byte 32 0) (- end start)))
                  (out (make-octet-buffer (+ pos 8))))
             (declare (type fixnum pos))
             (replace out (bw-buffer writer) :end2 pos)
             (setf (aref out pos)       (ldb (byte 8 0) crc)
                   (aref out (+ pos 1)) (ldb (byte 8 8) crc)
                   (aref out (+ pos 2)) (ldb (byte 8 16) crc)
                   (aref out (+ pos 3)) (ldb (byte 8 24) crc)
                   (aref out (+ pos 4)) (ldb (byte 8 0) size)
                   (aref out (+ pos 5)) (ldb (byte 8 8) size)
                   (aref out (+ pos 6)) (ldb (byte 8 16) size)
                   (aref out (+ pos 7)) (ldb (byte 8 24) size))
             out))
      (release-lz77-scratch scratch))))

(defun gzip-data-start (input start end)
  "Validate the gzip header in INPUT[START,END) and return the position of
  the first DEFLATE byte."
  (let ((n (- end start)))
    (unless (>= n 18)
      (error 'newzlib-format-error :detail "gzip stream shorter than header"))
    (unless (and (= (aref input start) #x1F) (= (aref input (1+ start)) #x8B))
      (error 'newzlib-format-error :detail "invalid gzip magic"))
    (let ((cm (aref input (+ start 2)))
          (flg (aref input (+ start 3))))
      (unless (= cm 8)
        (error 'newzlib-format-error :detail "unsupported gzip compression method"))
      (let ((pos (+ start 10)))
        (when (logbitp 2 flg)                       ; FEXTRA
          (unless (<= (+ pos 2) end)
            (error 'newzlib-format-error :detail "truncated gzip FEXTRA"))
          (let ((xlen (logior (aref input pos) (ash (aref input (1+ pos)) 8))))
            (incf pos (+ 2 xlen))))
        (when (logbitp 3 flg)                       ; FNAME
          (loop while (and (< pos end) (not (zerop (aref input pos))))
                do (incf pos))
          (incf pos))                               ; skip NUL
        (when (logbitp 4 flg)                       ; FCOMMENT
          (loop while (and (< pos end) (not (zerop (aref input pos))))
                do (incf pos))
          (incf pos))                               ; skip NUL
        (when (logbitp 1 flg)                       ; FHCRC
          (incf pos 2))
        (when (> pos end)
          (error 'newzlib-format-error :detail "gzip header overruns input"))
        pos))))

(defun gzip-decompress (input &optional (start 0) (end (length input)))
  "Decompress an RFC 1952 gzip stream INPUT[START,END).  Validates the header
  CRC32 and ISIZE.  Returns a fresh octet vector."
  (declare (type simple-array input)
           (type fixnum start end))
  (let ((data-start (gzip-data-start input start end)))
    (unless (>= end (+ data-start 8))
      (error 'newzlib-format-error :detail "gzip stream missing trailer"))
    (let* ((trailer (- end 8))
           (out (inflate-raw input data-start trailer))
           (expected-crc (logior (aref input trailer)
                                 (ash (aref input (1+ trailer)) 8)
                                 (ash (aref input (+ trailer 2)) 16)
                                 (ash (aref input (+ trailer 3)) 24)))
           (expected-size (logior (aref input (+ trailer 4))
                                  (ash (aref input (+ trailer 5)) 8)
                                  (ash (aref input (+ trailer 6)) 16)
                                  (ash (aref input (+ trailer 7)) 24))))
      (let ((actual-crc (crc32 out)))
        (unless (= actual-crc expected-crc)
          (error 'newzlib-format-error :detail "gzip CRC-32 mismatch")))
      (unless (= (logand (length out) #xFFFFFFFF) expected-size)
        (error 'newzlib-format-error :detail "gzip ISIZE mismatch"))
      out)))
