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
or the `compression-level-*` constants).  Pass `:mode :fast` to select the
faster greedy matcher, which trades compression ratio for throughput.

Format-specific wrappers:

```lisp
cl-newzlib:raw-deflate / raw-inflate     ; RFC 1951
cl-newzlib:zlib-compress / zlib-decompress  ; RFC 1950
cl-newzlib:gzip-compress / gzip-decompress  ; RFC 1952
```

## Allocation-free API

When the output size is already known, the `into` variants decode or encode
into a caller-owned buffer and return the number of octets written, with no
per-call allocation on the hot path:

```lisp
(cl-newzlib:decompress-into buffer octets :format :zlib) ; => count
(cl-newzlib:compress-into buffer octets :level 6 :mode :fast) ; => count
```

`buffer` must be a simple `(unsigned-byte 8)` vector that does not alias the
input; elements past the returned count are left untouched.

`decompress-into` requires capacity for the whole result and signals
`newzlib-parameter-error` if `buffer` is too small.  `compress-into` instead
needs worst-case capacity for the format, i.e. `(+ n (ash n -3))` plus the
wrapper overhead (`+6` for zlib, `+18` for gzip, `+256` for raw) — or, at
level 0, exactly `stored-block-octets n` plus that same overhead.  Like the
one-shot calls, these raise `newzlib-parameter-error` for a bad or aliased
buffer, and `newzlib-format-error` for corrupt input or a checksum mismatch,
in which case `buffer` is left partially written.

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

`cffi` is a hard dependency of the test system, and the cross-validation
tests additionally need a loadable `libz.so.1`; each one is skipped when
`zlib-available-p` is false, so the rest of the suite still runs.

## Status

The full suite passes: 1497 checks, 0 failures.
