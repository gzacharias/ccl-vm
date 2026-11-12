#!/bin/bash
# usage: ccl-tests.sh LISP [BASELINE]
# Runs Clozure/ccl-tests (in ./ccl-tests, cloned at $CCL_TESTS_REF, default the latest commit, if
# it isn't there) with the native ccl LISP (a kernel, with its image next to it).  Log:
# ccl-tests.log.  When the suite finishes, writes the names of the failing tests, sorted, to
# ccl-tests-failures.txt, after a "# ccl-tests COMMIT" line.
# Without BASELINE, fails if the suite doesn't finish or any test fails.
# With BASELINE (upstream-base's ccl-tests-failures.txt for the same ccl-tests commit, from
# ccl-tests-baseline.yml), fails only if the suite doesn't finish or a test fails here that
# passes on upstream-base; a test that fails only on upstream-base is a ::notice::.  The
# differences go to the GitHub step summary.
set -u
export LC_ALL=C   # the same sort order everywhere, for comm
lisp=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
baseline=${2:-}
if [ ! -d ccl-tests ]; then
  git clone -q https://github.com/Clozure/ccl-tests.git ccl-tests || exit 1
  if [ -n "${CCL_TESTS_REF:-}" ]; then git -C ccl-tests checkout -q "$CCL_TESTS_REF" || exit 1; fi
fi
commit=$(git -C ccl-tests rev-parse HEAD)
status=0
make -C ccl-tests CCL="$lisp" > ccl-tests.log 2>&1 || status=$?
tail -60 ccl-tests.log
summary() { echo "$*"; [ -n "${GITHUB_STEP_SUMMARY:-}" ] && echo "$*" >> "$GITHUB_STEP_SUMMARY"; return 0; }

rm -f ccl-tests-failures.txt
if ! grep -a -q -E '^=+ (All tests succeeded|Test suite failed) =+$' ccl-tests.log; then
  summary "- ccl-tests (ccl-tests $commit): didn't finish (exit status $status)"
  echo "::error::ccl-tests didn't finish (exit status $status), see ccl-tests.log"
  exit 1
fi
failed=$(grep -a -E '^Test [^ ]+ failed$' ccl-tests.log | sed -E 's/^Test ([^ ]+) failed$/\1/' | sort -u)
# The suite's own count must agree with the names found.
reported=$(sed -n -E 's/^([0-9]+) out of [0-9]+ total tests failed.*/\1/p' ccl-tests.log | tail -1)
count=$(printf '%s' "$failed" | grep -c .)
if [ "${reported:-0}" != "$count" ]; then
  echo "::error::ccl-tests reports ${reported:-0} failures, but ccl-tests.sh found $count 'Test ... failed' lines"
  exit 1
fi
{ echo "# ccl-tests $commit"; [ -n "$failed" ] && echo "$failed"; } > ccl-tests-failures.txt

if [ -z "$baseline" ]; then
  summary "- ccl-tests (ccl-tests $commit): $count failed${failed:+: $(echo $failed)}"
  exit $status
fi

[ -f "$baseline" ] || { echo "::error::no ccl-tests baseline $baseline"; exit 1; }
base_commit=$(sed -n 's/^# ccl-tests //p' "$baseline")
if [ "$base_commit" != "$commit" ]; then
  echo "::error::the baseline is for ccl-tests $base_commit, but this run used $commit"
  exit 1
fi
names() { grep -v '^#' "$1"; }
new=$(comm -23 <(names ccl-tests-failures.txt) <(names "$baseline"))
fixed=$(comm -13 <(names ccl-tests-failures.txt) <(names "$baseline"))
same=$(comm -12 <(names ccl-tests-failures.txt) <(names "$baseline") | grep -c .)
summary "- ccl-tests (ccl-tests $commit, vs $(sed -n 's/^# upstream-base /upstream-base /p' "$baseline")): $count failed, $same of them also on upstream-base"
for t in $fixed; do
  echo "::notice::ccl-test $t passes here but fails on upstream-base"
  summary "  - passes here, fails on upstream-base: $t"
done
for t in $new; do
  echo "::error::ccl-test $t fails here but passes on upstream-base"
  summary "  - **fails here, passes on upstream-base: $t**"
done
[ -z "$new" ]
