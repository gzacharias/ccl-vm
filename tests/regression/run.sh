#!/bin/bash
# usage: run.sh native CCL-DIR [TEST...]
#        run.sh HOST BUNDLE-DIR [TEST...]
# Runs the regression tests in this directory (all, or the named ones, e.g. floats exit-status)
# natively with the build in CCL-DIR, or in ccl-vm on HOST (sbcl, ccl) booted from BUNDLE-DIR.
# A NAME.lisp test is fed, after harness.lisp, to the CCL listener and passes when it prints
# REGRESSION PASS lines, no REGRESSION FAIL line, and finishes.  A NAME.sh test is run with
# the same two arguments and passes on exit status 0 (77: skipped here).
# Logs: regression-LABEL/NAME.log, LABEL being native or HOST-<bundle dir name>.
# Test failures are reported (output, ::warning:: annotations, the GitHub step summary), not
# turned into the exit status: it is 1 only when the harness itself breaks (no such test, or a
# NAME.lisp log without the line harness.lisp prints once it is loaded).  See README.md.
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

failed=() broken=()
for test in "${tests[@]}"; do
  log=$logs/$test.log
  if [ -f "$here/$test.lisp" ]; then
    run_lisp "$here/$test.lisp" "$log"
    if ! grep -qx 'REGRESSION HARNESS READY' "$log"; then
      echo "BROKEN $test: harness.lisp didn't load (log $log)"
      echo "::error::regression harness didn't load for $test ($label), see $log"
      broken+=("$test"); continue
    fi
    if grep -qx 'REGRESSION DONE' "$log"; then outcome=done; else outcome=unfinished; fi
  elif [ -f "$here/$test.sh" ]; then
    "$here/$test.sh" "$where" "$dir" > "$log" 2>&1
    case $? in 0) outcome=done ;; 77) outcome=skipped ;; *) outcome=failed ;; esac
  else
    echo "::error::no regression test $test in $here"; broken+=("$test"); continue
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
    echo "::warning::regression test $test failed ($label), see $log"
    failed+=("$test")
  fi
done
if [ ${#failed[@]} -gt 0 ]; then summary="failed: ${failed[*]}"; else summary="all passed"; fi
[ ${#broken[@]} -gt 0 ] && summary="$summary; harness broken: ${broken[*]}"
echo "regression tests ($label): $summary"
[ -n "${GITHUB_STEP_SUMMARY:-}" ] && echo "- regression tests ($label): $summary" >> "$GITHUB_STEP_SUMMARY"
[ ${#broken[@]} -eq 0 ]
