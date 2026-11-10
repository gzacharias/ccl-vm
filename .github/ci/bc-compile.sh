#!/bin/bash
# usage: bc-compile.sh [CCL-DIR]
# Compiles the bc bundle with the native build in CCL-DIR (default ./ccl) into CCL-DIR/ccl-bc/
# and packs it as ./ccl-bc.tar.gz.  Log: bc.log.
set -u
. "$(dirname "$0")/platform.sh"
dir=${1:-ccl}
log=$PWD/bc.log
(cd "$dir" && ./$CCL_KERNEL -n -b > "$log" 2>&1 <<'EOF') || true
(load "ccl:tools;xdarwincvm.lisp")
(progn
  (ccl:bc-compile-ccl :force t :output "ccl:ccl-bc;")
  (format t "~&BC-DONE~%"))
(quit)
EOF
cat "$log"
grep -qx BC-DONE "$log" || { echo "::error::bc-compile-ccl didn't finish"; exit 1; }
echo "bc files: $(find "$dir/ccl-bc" -name '*.bc' | wc -l | tr -d ' ')"
tar -czf ccl-bc.tar.gz -C "$dir" ccl-bc
