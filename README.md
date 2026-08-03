# cl-newzlib

A zlib-compatible DEFLATE compression library in Common Lisp.

Implements all three related formats in portable Common Lisp:

* **RFC 1951 — DEFLATE** (raw streams; level 0 = stored blocks; levels
  1-3 = greedy LZ77, 4-9 = lazy matching, all with fixed or dynamic
  Huffman coding)
* **RFC 1950 — zlib wrapper**
* **RFC 1952 — gzip wrapper**

The compressor uses a faithful port of zlib's `build_tree`/`gen_bitlen`/
`gen_codes` (trees.c) so emitted dynamic trees are exactly valid DEFLATE
trees (max 15 bits for literals/lengths, 7 for code lengths), and the
inflater builds canonical decode tables with a Kraft-inequality check.

## Dependencies

* [iterate](https://github.com/Shinmera/iterate)
* [metabang-bind](https://github.com/gwkkwg/metabang-bind)

Both are available through Quicklisp.

## One-shot API

```lisp
(cl-newzlib:compress-octets octets :format :zlib)   ; or :gzip / :raw
(cl-newzlib:decompress-octets octets :format :zlib)
```

`cl-newzlib:compress` / `cl-newzlib:decompress` also accept a pathname or
binary stream, and dispatch on the `:format` keyword (`:zlib`, `:gzip`,
`:raw`).  Compression level is selectable with
`:level` (`+no-compression+`, `+default-compression+`, `+best-compression+`
or the `compression-level-*` constants).

Format-specific wrappers:

```lisp
cl-newzlib:raw-deflate / raw-inflate     ; RFC 1951
cl-newzlib:zlib-compress / zlib-decompress  ; RFC 1950
cl-newzlib:gzip-compress / gzip-decompress  ; RFC 1952
```

## Streaming API

```lisp
cl-newzlib:make-deflate-stream ; / deflate-stream-write / deflate-stream-finish / deflate-stream-end
cl-newzlib:make-inflate-stream ; / inflate-stream-read / inflate-stream-eof-p / inflate-stream-end
```

## Checksums

```lisp
cl-newzlib:adler32
cl-newzlib:crc32
```

## Tests

The test suite validates the library with [FiveAM](https://github.com/sionescu/fiveam)
and cross-checks against the system C zlib via CFFI (raw/zlib/gzip roundtrips
in both directions, checksum comparison, window/boundary behaviour).

```lisp
(asdf:load-system "cl-newzlib-tests")
(cl-newzlib-tests:run-tests)
```

Requires a `libz.so` available to CFFI for the cross-validation tests; the
rest of the suite runs without it.

## Status

The full suite passes: 1421 checks, 0 failures.
