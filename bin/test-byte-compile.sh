#!/usr/bin/env bash
set -eu -o pipefail

tmpdir=$(mktemp --directory)
cp *.el "$tmpdir"
pushd "$tmpdir" >/dev/null

echo "==> Byte compiling tree-guide.el with $(emacs --version | head -n 1) in $tmpdir"

# Note: `(setq byte-compile-error-on-warn t)` causes `emacs` to return
# non-zero exit status if there any warnings or errors during byte
# compilation.

emacs --quick \
      --batch \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile \
      *.el

echo "==> PASS"
