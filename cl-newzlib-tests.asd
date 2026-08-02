(asdf:defsystem "cl-newzlib-tests"
  :version "0.0.1.0"
  :description "Test system for cl-newzlib, including cross-validation against system zlib."
  :depends-on ("cl-newzlib"
               "fiveam"
               "cffi")
  :serial t
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "zlib-ffi")
                             (:file "util-tests")
                             (:file "bit-io-tests")
                             (:file "huffman-tests")
                             (:file "roundtrip-tests")
                             (:file "cross-tests")))))
