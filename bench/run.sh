#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
lisp="${BENCH_LISP:-sbcl}"
exec "$lisp" --script "$root/bench/run.lisp" "$@"
