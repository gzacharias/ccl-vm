#!/bin/bash
# usage: run.sh native CCL-DIR [TEST...]
#        run.sh HOST BUNDLE-DIR [TEST...]
# Runs the regression tests in this directory (all, or the named ones, e.g. floats exit-status)
# natively with the build in CCL-DIR, or in ccl-vm on HOST (sbcl, ccl) booted from BUNDLE-DIR.
# A NAME.lisp test is fed, after harness.lisp, to the CCL listener and passes when it prints
# REGRESSION PASS lines, no REGRESSION FAIL line, and finishes.  A NAME.sh test is run with
# the same two arguments and passes on exit status 0 (77: skipped here).
# Logs: regression-LABEL/NAME.log, LABEL being native or HOST-<bundle dir name>.
# Exits 1 when any test fails.  See README.md.
set -u
here=$(cd "$(dirname "$0")" && pwd)
. "$here/../../.github/ci/platform.sh"
where=$1 dir=$2
shift 2
if [ "$where" = native ]; then label=native; else label=$where-$(basename "$dir"); fi
logs=$PWD/regression-$label
mkdir -p "$logs"

if [ $# -gt 0 ]; then tests=("$@")
else tests=($(cd "$here" && ls *.lisp *.sh | grep -v -x -e harness.lisp -e run.sh | sed 's/\.[a-z]*$//' | sort -u))
fi

run_lisp() { # run_lisp FILE LOG
  { cat "$here/harness.lisp" "$1"; echo '(format t "~&REGRESSION DONE~%")'
    [ "$where" = native ] && echo '(quit)'; } |
  if [ "$where" = native ]; then (cd "$dir" && ./$CCL_KERNEL -n -b)
  else "$here/../../.github/ci/vm" "$where" "$dir"
  fi > "$2" 2>&1
}

failed=()
for test in "${tests[@]}"; do
  log=$logs/$test.log
  if [ -f "$here/$test.lisp" ]; then
    run_lisp "$here/$test.lisp" "$log"
    if grep -qx 'REGRESSION DONE' "$log"; then outcome=done; else outcome=unfinished; fi
  elif [ -f "$here/$test.sh" ]; then
    "$here/$test.sh" "$where" "$dir" > "$log" 2>&1
    case $? in 0) outcome=done ;; 77) outcome=skipped ;; *) outcome=failed ;; esac
  else
    echo "::error::no regression test $test in $here"; failed+=("$test"); continue
  fi
  passes=$(grep -a -c '^REGRESSION PASS' "$log")
  failures=$(grep -a '^REGRESSION FAIL' "$log")
  if [ "$outcome" = skipped ]; then
    echo "skip $test (not for $label)"
  elif [ "$outcome" = done ] && [ -z "$failures" ] && [ "$passes" -gt 0 ]; then
    echo "ok   $test ($passes checks)"
  else
    echo "FAIL $test ($passes checks passed, $outcome; log $log)"
    [ -n "$failures" ] && echo "$failures" | sed 's/^/     /'
    echo "::error::regression test $test failed ($label), see $log"
    failed+=("$test")
  fi
done
if [ ${#failed[@]} -gt 0 ]; then echo "regression tests ($label): failed: ${failed[*]}"; exit 1; fi
echo "regression tests ($label): all passed"
