#!/bin/bash
# The vm script exits with CCL's exit status: at end of input, after (quit N), after quitting
# from a break loop, and after (toplevel) from a break loop.
# Guards: ccl-vm 67ce367 (the vm script exits with the value CLOOP returns).  VM only.
# Called by run.sh as: exit-status.sh native CCL-DIR | exit-status.sh HOST BUNDLE-DIR
set -u
where=$1 dir=$2
[ "$where" = native ] && exit 77
vm=$(cd "$(dirname "$0")/../../.github/ci" && pwd)/vm
status=0
# Each case: forms separated by |, then the expected exit status.
for c in '(print :eof)|0' '(quit 3)|3' '(error "e")|:q|(quit 5)|5' '(error "e")|:pop|(toplevel)|(quit 6)|6'; do
  expect=${c##*|} forms=${c%|*}
  echo "$forms" | tr '|' '\n' | "$vm" "$where" "$dir" >&2  # the test's log
  got=$?
  if [ "$got" = "$expect" ]; then echo "REGRESSION PASS [$forms] exit $got"
  else echo "REGRESSION FAIL [$forms]: exit $got, expected $expect"; status=1; fi
done
exit $status
