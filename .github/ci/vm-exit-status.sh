#!/bin/bash
# usage: vm-exit-status.sh HOST BUNDLE-DIR
# Checks that the vm script exits with CCL's exit status on HOST: at end of input, after
# (quit N), after quitting from a break loop, and after (toplevel) from a break loop.
# Log: exit-HOST.log (the last case's).
set -u
here=$(cd "$(dirname "$0")" && pwd)
host=$1 bundle=$2
status=0
# Each case: forms separated by |, then the expected exit status.
for c in '(print :eof)|0' '(quit 3)|3' '(error "e")|:q|(quit 5)|5' '(error "e")|:pop|(toplevel)|(quit 6)|6'; do
  expect=${c##*|} forms=${c%|*}
  echo "$forms" | tr '|' '\n' | "$here/vm" "$host" "$bundle" > exit-$host.log 2>&1
  got=$?
  if [ "$got" = "$expect" ]; then echo "ok: [$forms] exit $got"
  else echo "::error::vm $host [$forms] exited $got, expected $expect"; status=1; fi
done
exit $status
