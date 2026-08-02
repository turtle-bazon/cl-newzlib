(defpackage #:cl-newzlib
  (:use #:cl)
  (:local-nicknames (#:it #:iterate))
  (:import-from #:metabang-bind
                #:bind)
  (:export
   ;; Errors
   #:newzlib-error
   #:newzlib-parameter-error
   #:newzlib-format-error
   #:newzlib-unsupported-error
   #:newzlib-memory-error
   #:newzlib-end-of-input
   ;; Checksums
   #:adler32
   #:crc32
   ;; One-shot API
   #:compress-octets
   #:decompress-octets
   #:compress
   #:decompress
   ;; Format wrappers
   #:zlib-compress
   #:zlib-decompress
   #:gzip-compress
   #:gzip-decompress
   #:raw-deflate
   #:raw-inflate
   ;; Compression level / strategy constants
   #:compression-level-no-compression
   #:compression-level-fastest
   #:compression-level-default
   #:compression-level-best
   #:+no-compression+
   #:+default-compression+
   #:+best-compression+
   ;; Streaming API
   #:deflate-stream
   #:make-deflate-stream
   #:deflate-stream-write
   #:deflate-stream-finish
   #:deflate-stream-end
   #:inflate-stream
   #:make-inflate-stream
   #:inflate-stream-read
   #:inflate-stream-eof-p
   #:inflate-stream-end
   ;; Version
   #:version))
