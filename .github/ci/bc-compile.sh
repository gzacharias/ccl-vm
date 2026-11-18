#!/bin/bash
# usage: [BC_TARGET=linuxcvm] [BC_NAME=NAME] bc-compile.sh [CCL-DIR]
# Compiles the bc bundle for BC_TARGET (default darwincvm, on any host) with the native build in
# CCL-DIR (default ./ccl) into CCL-DIR/NAME/ and packs it as ./NAME.tar.gz (NAME: default ccl-bc).
# Log: NAME.log (bc.log for ccl-bc).
set -u
. "$(dirname "$0")/platform.sh"
dir=${1:-ccl}
name=${BC_NAME:-ccl-bc}
log=$PWD/$([ "$name" = ccl-bc ] && echo bc || echo "$name").log
(cd "$dir" && ./$CCL_KERNEL -n -b > "$log" 2>&1 <<EOF) || true
(load "ccl:tools;xdarwincvm.lisp")
(progn
  (ccl:bc-compile-ccl :force t :output "ccl:$name;" :target :${BC_TARGET:-darwincvm})
  (format t "~&BC-DONE~%"))
(quit)
EOF
cat "$log"
grep -qx BC-DONE "$log" || { echo "::error::bc-compile-ccl didn't finish"; exit 1; }
echo "bc files: $(find "$dir/$name" -name '*.[dl]bc' | wc -l | tr -d ' ')"
COPYFILE_DISABLE=1 tar -czf "$name.tar.gz" -C "$dir" "$name"
