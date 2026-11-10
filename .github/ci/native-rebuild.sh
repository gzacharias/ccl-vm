#!/bin/bash
# usage: native-rebuild.sh GENERATION [CCL-DIR] [--fail-on-warnings]
# Runs (rebuild-ccl :clean t) natively in CCL-DIR (default ./ccl).  GENERATION 1 is the first
# rebuild, by the 1.14 binaries (it loads compile-ccl.lisp first, which redefines kernel
# functions); 2 is the rebuild by generation 1, which should be warning-free.  Log:
# buildGENERATION.log.  Fails if the rebuild doesn't finish; compiler warnings print a GitHub
# warning, and with --fail-on-warnings also fail.
set -u
. "$(dirname "$0")/platform.sh"
gen=$1 dir=ccl fail_on_warnings=false
shift
for arg in "$@"; do
  case $arg in
    --fail-on-warnings) fail_on_warnings=true ;;
    *) dir=$arg ;;
  esac
done
log=$PWD/build$gen.log
prologue=
[ "$gen" = 1 ] && prologue='(let ((*warn-if-redefine-kernel* nil)) (load "ccl:lib;compile-ccl.lisp"))'
(cd "$dir" && ./$CCL_KERNEL -n -b > "$log" 2>&1 <<EOF) || true
$prologue
(progn
  (rebuild-ccl :clean t)
  (format t "~&BUILD$gen-DONE~%"))
(quit)
EOF
cat "$log"
# Whole-line match: a backtrace can quote the form's source.
grep -qx "BUILD$gen-DONE" "$log" || { echo "::error::rebuild $gen didn't finish"; exit 1; }
if grep -q ';Compiler warnings' "$log"; then
  echo "::warning::rebuild $gen has compiler warnings"
  $fail_on_warnings && exit 1
fi
exit 0
