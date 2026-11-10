#!/bin/bash
# CLOOP can be called again after CCL quits: QUIT sets the toplevel function to NIL, and the
# second CLOOP used to say "CCL isn't loaded".  Both CLOOPs read the same stdin and return the
# exit status of their QUIT.
# Guards: ccl-vm 1b99b1b (remember the toplevel function so cloop can restart).  VM only, sbcl and
# ccl hosts (runs the host itself: the vm script calls CLOOP once).
# Called by run.sh as: cloop-restart.sh native CCL-DIR | cloop-restart.sh HOST BUNDLE-DIR
set -u
out=$(mktemp)
trap 'rm -f "$out"' EXIT
where=$1 bundle=$(cd "$2" && pwd)
prologue=$(cd "$(dirname "$0")/../../.github/ci" && pwd)/vm-prologue.lisp
load="(ccl-vm:load-ccl #p\"$bundle/\")"
first='(format t "~&FIRST-EXIT ~a~%" (ccl-vm:cloop))'
second='(format t "~&SECOND-EXIT ~a~%" (ccl-vm:cloop))'
input='(quit)
(format t "~&SECOND-CLOOP ~a~%" (+ 1 2))
(quit 4)'
case $where in
  sbcl) echo "$input" | ${SBCL:-sbcl} --dynamic-space-size 12GB --control-stack-size 512MB --noinform \
          --non-interactive --load "$prologue" --eval "$load" --eval "$first" --eval "$second" ;;
  ccl)  echo "$input" | "$CCL_HOST" --no-init --batch \
          --load "$prologue" --eval "$load" --eval "$first" --eval "$second" --eval '(ccl:quit)' ;;
  *) exit 77 ;;
esac > "$out" 2>&1
cat "$out" >&2   # the test's log
status=0
for line in 'FIRST-EXIT 0' 'SECOND-CLOOP 3' 'SECOND-EXIT 4'; do
  if grep -aqx "$line" "$out"; then echo "REGRESSION PASS $line"
  else echo "REGRESSION FAIL $line: missing"; status=1; fi
done
exit $status
