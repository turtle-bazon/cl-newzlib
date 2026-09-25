(in-package #:cl-newzlib)

;;; RFC 1950 zlib wrapper format.
;;;
;;;   +---+---+
;;;   |CMF|FLG|   (2 bytes)
;;;   +---+---+
;;;   |     DEFLATE DATA     |
;;;   +---+---+---+---+
;;;   |    Adler-32      |   (4 bytes, big-endian)
;;;   +---+---+---+---+
;;;
;;; CMF: CM = 8 (DEFLATE), CINFO = 7 (32K window) => 0x78.
;;; FLG: FCHECK is chosen so that (CMF*256 + FLG) mod 31 == 0.  FDICT is
;;; always 0 (no preset dictionary support).

(defun zlib-flevel (level)
  (cond ((< level 2) 0)
        ((< level 6) 1)
        ((= level 6) 2)
        (t 3)))

(defun zlib-header-octets (level)
  "Return the two zlib header octets for the given compression LEVEL."
  (let* ((cmf #x78)
         (flevel (zlib-flevel level))
         (fcheck (mod (- 31 (mod (+ (* cmf 256) (ash flevel 6)) 31)) 31)))
    (make-array 2 :element-type '(unsigned-byte 8)
                :initial-contents (list cmf (logior (ash flevel 6) fcheck)))))

(defun %zlib-stored-compress (input start end level)
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end level))
  (let* ((n (- end start))
         (out (make-octet-buffer (+ 6 (stored-block-octets n))))
         (writer (make-bit-writer-for-buffer out)))
    (setf (bw-pos writer) 2)
    (let ((header (zlib-header-octets level))
          (buf (bw-buffer writer)))
      (setf (aref buf 0) (aref header 0)
            (aref buf 1) (aref header 1)))
    (emit-stored-blocks writer input start end)
    (flush-bits writer)
    (let* ((pos (bw-pos writer))
           (checksum (adler32 input start end)))
      (declare (type fixnum pos))
      (setf (aref out pos)       (ldb (byte 8 24) checksum)
            (aref out (+ pos 1)) (ldb (byte 8 16) checksum)
            (aref out (+ pos 2)) (ldb (byte 8 8) checksum)
            (aref out (+ pos 3)) (ldb (byte 8 0) checksum))
      out)))

(defun zlib-compress (input &optional (start 0) (end (length input))
                            (level +default-compression+))
  "Compress INPUT[START,END) into the RFC 1950 zlib format (header + DEFLATE
data + Adler-32).  Returns a fresh octet vector."
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end))
  (check-compression-level level)
  (if (zerop level)
      (%zlib-stored-compress input start end level)
      (let* ((n (- end start))
             (scratch (acquire-lz77-scratch (1+ n)))
             (writer (reset-bit-writer (lzs-writer scratch)
                                       (+ n (ash n -3) 262))))
        (unwind-protect
             (progn
               (setf (bw-pos writer) 2)
               (let ((header (zlib-header-octets level))
                     (buf (bw-buffer writer)))
                 (setf (aref buf 0) (aref header 0)
                       (aref buf 1) (aref header 1)))
               (deflate-into-writer input start end writer level scratch)
               (flush-bits writer)
               (let* ((pos (bw-pos writer))
                      (checksum (adler32 input start end))
                      (out (make-octet-buffer (+ pos 4))))
                 (declare (type fixnum pos))
                 (replace out (bw-buffer writer) :end2 pos)
                 (setf (aref out pos)       (ldb (byte 8 24) checksum)
                       (aref out (+ pos 1)) (ldb (byte 8 16) checksum)
                       (aref out (+ pos 2)) (ldb (byte 8 8) checksum)
                       (aref out (+ pos 3)) (ldb (byte 8 0) checksum))
                 out))
          (release-lz77-scratch scratch)))))

(defun parse-zlib-header (input start end)
  "Validate the zlib header in INPUT[START,END).  Returns the position just
  past the header (including any dictionary id)."
  (let ((n (- end start)))
    (unless (>= n 2)
      (error 'newzlib-format-error :detail "zlib stream shorter than header"))
    (let ((cmf (aref input start))
          (flg (aref input (1+ start))))
      (unless (= (logand cmf #x0F) 8)
        (error 'newzlib-format-error :detail "unsupported zlib compression method"))
      (unless (= (logand (ash cmf -4) #x0F) 7)
        (error 'newzlib-format-error :detail "unsupported zlib window size"))
      (unless (zerop (mod (+ (* cmf 256) flg) 31))
        (error 'newzlib-format-error :detail "zlib header checksum mismatch"))
      (unless (zerop (logand flg #x20))
        (error 'newzlib-unsupported-error :detail "zlib preset dictionaries are not supported"))
      (if (logbitp 5 flg)
          (+ start 6)
          (+ start 2)))))

(defun zlib-decompress (input &optional (start 0) (end (length input)))
  "Decompress an RFC 1950 zlib stream INPUT[START,END).  Validates the header
  and Adler-32 checksum.  Returns a fresh octet vector."
  (declare (type simple-array input)
           (type fixnum start end))
  (let ((data-start (parse-zlib-header input start end)))
    (unless (>= end (+ data-start 4))
      (error 'newzlib-format-error :detail "zlib stream missing Adler-32"))
    (let* ((deflated-end (- end 4))
           (out (inflate-raw input data-start deflated-end))
           (expected (logior (ash (aref input deflated-end) 24)
                             (ash (aref input (1+ deflated-end)) 16)
                             (ash (aref input (+ deflated-end 2)) 8)
                             (aref input (+ deflated-end 3)))))
      (let ((actual (adler32 out)))
        (unless (= actual expected)
          (error 'newzlib-format-error :detail "zlib Adler-32 mismatch")))
      out)))
