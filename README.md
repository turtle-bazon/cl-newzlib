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
* [sb-cpu-features](https://developer.bazon.ru/common-lisp/sb-cpu-features) — SBCL only

All are available through Quicklisp.  On SBCL, `sb-cpu-features` supplies
CPUID/XGETBV-based feature detection and image-restart re-probing.

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

## Benchmarks

`bench/run.sh` compares cl-newzlib against the system C zlib on identical
input.  It measures both the one-shot API and the allocation-free `into`
API, reports MiB/s of uncompressed data plus compressed size, and
cross-checks every stream (cl-newzlib output is inflated by zlib and vice
versa) before timing it.

```sh
bench/run.sh                 # 2 MiB datasets, levels 1/3/6/9, 3 samples
bench/run.sh --quick         # 1 MiB, levels 1/6, 1 sample
bench/run.sh --size 8MiB --levels 1,6,9 --samples 5
bench/run.sh --datasets text,binary   # realistic workloads only
bench/run.sh --format gzip   # or raw
bench/run.sh --mode fast     # cl-newzlib's greedy matcher
bench/run.sh --help

# e.g. compare a second SBCL built through Roswell
BENCH_LISP="$HOME/.roswell/impls/x86-64/linux/sbcl-bin/2.6.9/bin/sbcl" \
  bench/run.sh
```

The default datasets are real text (`/usr/share/doc/rclone/MANUAL.html`),
a real binary (`/usr/local/bin/sbcl`), zeroes, and deterministic random
bytes.  Missing system files fall back to the repository source or
`/usr/bin/bash`, so the harness also runs on other machines.  The source
tree can be benchmarked with `--datasets source`.

Reported `cl/zlib` values are cl-newzlib's throughput divided by zlib's,
so values above 1 mean cl-newzlib is faster.  `--mode fast` changes only
cl-newzlib's matcher; zlib is always run with its default strategy.

AVX2, SSE4.1 and PCLMULQDQ code is compiled whenever the sb-simd packages
are available, including on a build machine without those CPU features.
Detection goes through `sb-cpu-features` on SBCL.  Every entry point is
selected at runtime: an image built on a non-AVX2 machine can use its AVX2
code when moved to an AVX2 host, and an image built on AVX2 disables those
paths when restarted on a lesser CPU.  The AVX2 check includes OS YMM state
(SBCL's XGETBV-backed flag), not just CPUID.

Compile-time constant folding is the one trap: SBCL may evaluate AVX2
operations during compilation if their arguments are constant.  On an
AVX2-less build host the AVX2 kernels are therefore compiled with every
AVX2 operation declared `notinline`; on an AVX2 build host the fast VOPs
stay enabled.  The Adler zero pack is also sourced from a runtime variable
and its constructor is always `notinline`.  Any new AVX2 code should keep
constant-foldable arguments out of AVX2 constructor calls.

On the development machine (SBCL 2.6.7 and 2.6.9, zlib 1.2.13, 2 MiB
text/binary inputs), compression is now about 1.1-1.5x zlib at levels
3/6/9 (level 9 reaches several times zlib on binary data).  Decompression
is at parity on text and about 0.9x on binary; one-shot decompression is
around 0.9-1.0x.  Binary inflate is the remaining small gap.

## Status

The full suite passes on SBCL 2.6.7 and 2.6.9 (1572 checks with AVX2 compiled;
the non-AVX2 build configuration is also tested).
