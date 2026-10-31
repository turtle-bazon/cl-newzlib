(asdf:defsystem "cl-newzlib-bench"
  :version "0.0.1.0"
  :description "Benchmark cl-newzlib against the system C zlib."
  :depends-on ("cl-newzlib"
               "cffi")
  :serial t
  :components ((:module "bench"
                :components ((:file "bench")))))
