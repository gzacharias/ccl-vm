#!/bin/bash
# usage: run-checks.sh native [CCL-DIR]
#        run-checks.sh HOST BUNDLE-DIR [NATIVE-FLOAT-CHECKS]
# Runs checks/nan-checks.lisp and checks/float-checks.lisp natively (the build in CCL-DIR,
# default ./ccl) or in ccl-vm on HOST (sbcl, ccl) booted from BUNDLE-DIR, and writes their
# output lines to nan-checks-LABEL.txt and float-checks-LABEL.txt (LABEL: native, or
# HOST-BUNDLE).  The nan checks are only reported, and a missing CHECK-DONE is a warning.  The
# float checks must finish, and in the VM must print exactly what NATIVE-FLOAT-CHECKS (a native
# float-checks-native.txt) holds.  Logs: nan-checks-LABEL.log, float-checks-LABEL.log.
set -u
here=$(cd "$(dirname "$0")" && pwd)
. "$here/platform.sh"
where=$1
if [ "$where" = native ]; then
  dir=${2:-ccl} label=native reference=
else
  bundle=$2 label=$1-$(basename "$2") reference=${3:-}
fi
run() { # run CHECK-FILE LOG
  if [ "$where" = native ]; then
    { cat "$1"; echo '(quit)'; } | (cd "$dir" && ./$CCL_KERNEL -n -b) > "$2" 2>&1
  else
    "$here/vm" "$where" "$bundle" < "$1" > "$2" 2>&1
  fi
}
status=0
run "$here/checks/nan-checks.lisp" nan-checks-$label.log
grep -a '^CHECK' nan-checks-$label.log > nan-checks-$label.txt
cat nan-checks-$label.txt
grep -qx CHECK-DONE nan-checks-$label.txt || echo "::warning::the nan checks ($label) didn't finish, see nan-checks-$label.log"
run "$here/checks/float-checks.lisp" float-checks-$label.log
grep -a '^FCHECK' float-checks-$label.log > float-checks-$label.txt
echo "float checks ($label): $(wc -l < float-checks-$label.txt | tr -d ' ') lines"
grep -qx 'FCHECK DONE' float-checks-$label.txt || { echo "::error::the float checks ($label) didn't finish, see float-checks-$label.log"; status=1; }
if [ -n "$reference" ]; then
  if diff "$reference" float-checks-$label.txt; then echo "float checks ($label) == native"
  else echo "::error::the float checks ($label) differ from native"; status=1; fi
fi
exit $status
