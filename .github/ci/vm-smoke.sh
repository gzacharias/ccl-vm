#!/bin/bash
# usage: vm-smoke.sh HOST
# Boots ./ccl-bc in ccl-vm on HOST and checks that compiled code runs.  Log: smoke-HOST.log.
set -u
host=$1
log=smoke-$host.log
"$(dirname "$0")/vm" "$host" ccl-bc > "$log" 2>&1 <<'EOF'
(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(format t "~&VM-SMOKE ~a ~a~%" (fact 20) (funcall (compile nil '(lambda (l) (reduce #'+ l))) '(1 2 3)))
EOF
cat "$log"
# Whole-line match: a backtrace can quote the form's source.
grep -qx 'VM-SMOKE 2432902008176640000 6' "$log"
