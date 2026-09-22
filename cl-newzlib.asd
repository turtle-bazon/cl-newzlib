(asdf:defsystem "cl-newzlib"
  :version "0.0.1.0"
  :description "A zlib-compatible DEFLATE compression library in Common Lisp."
  :long-description
  "cl-newzlib implements RFC 1950 (zlib wrapper), RFC 1951 (DEFLATE) and
RFC 1952 (gzip wrapper) in portable Common Lisp with SBCL-specific
fast paths, aiming to compete with the C zlib implementation."
  :author "cl-newzlib contributors"
  :license "Zlib"
  :depends-on ("iterate"
               "metabang-bind")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "conditions")
                             (:file "util")
                             (:file "adler32")
                             (:file "crc32")
                             (:file "pclmul-crc")
                             (:file "bit-io")
                             (:file "huffman")
                             (:file "simd")
                             (:file "deflate")
                             (:file "inflate")
                             (:file "zlib-format")
                             (:file "gzip-format")
                             (:file "streams")
                             (:file "api")))))
