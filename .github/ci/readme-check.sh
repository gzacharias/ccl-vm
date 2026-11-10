#!/bin/bash
# usage: readme-check.sh HOST TAG
# Follows the README's "Running CCL in the VM" literally, as a new user would, on HOST (sbcl, or
# ccl via $CCL_HOST): downloads the latest released bundle and unpacks it, clones ccl-vm at TAG,
# and in a fresh HOST evaluates the README's forms as written (no heap-size or other options),
# then types the README's example into the listener.  Needs ~/quicklisp (install-hosts.sh).
# Works in ./readme-HOST/.  Log: readme-HOST.log.
set -u
host=$1 tag=$2
w=$PWD/readme-$host
log=$PWD/readme-$host.log
rm -rf "$w"; mkdir -p "$w"; cd "$w" || exit 1
curl -sSL -O https://github.com/gzacharias/ccl-vm/releases/latest/download/ccl-bc.tar.gz || exit 1
tar -xzf ccl-bc.tar.gz || exit 1
git clone -q --branch "$tag" https://github.com/gzacharias/ccl-vm.git ccl-vm || exit 1
echo "bundle: $(find ccl-bc -name '*.bc' | wc -l | tr -d ' ') bc files, sha256 $(shasum -a 256 ccl-bc.tar.gz | cut -c1-12); ccl-vm $(git -C ccl-vm rev-parse --short HEAD)"
# The README's host forms, one --eval each; stdin goes to the VM's listener only (the host's REPL
# would otherwise buffer part of it).
host_forms=(
  '(load "~/quicklisp/setup.lisp")'
  "(pushnew #P\"$w/ccl-vm/\" asdf:*central-registry* :test #'equal)"
  '(asdf:load-system "ccl-vm")'
  '(format t "~&SYSTEM-DIR ~a~%" (asdf:system-source-directory "ccl-vm"))'
  "(ccl-vm:load-ccl #P\"$w/ccl-bc/\")"
  '(ccl-vm:cloop)')
evals=(); for f in "${host_forms[@]}"; do evals+=(--eval "$f"); done
case $host in
  sbcl) cmd=("${SBCL:-sbcl}" --noinform --no-userinit) ;;
  ccl)  cmd=("$CCL_HOST" -n) ;;
  *) echo "unknown host $host" >&2; exit 1 ;;
esac
"${cmd[@]}" "${evals[@]}" --eval '(quit)' > "$log" 2>&1 <<'EOF'
(+ 1 2)
(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(format t "~&README-FACT ~a~%" (fact 20))
(#_exit 0)
nil
EOF
tail -20 "$log"
grep -q "SYSTEM-DIR $w/ccl-vm/" "$log" || { echo "::error::($host) ASDF didn't load ccl-vm from the clone"; exit 1; }
grep -qx 'README-FACT 2432902008176640000' "$log" || { echo "::error::($host) the README example didn't run"; exit 1; }
echo "$host: the README instructions work"
