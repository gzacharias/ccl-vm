#!/bin/bash
# usage: vm-cross-compile.sh HOST
# Boots ./ccl-bc in ccl-vm on HOST and cross-compiles a native darwinx8664 ccl from a copy of
# the sources in ./ccl/, then builds the kernel and checks that the result boots.
# Log: xc-HOST.log.
set -eu
host=$1
tree=xtree-$host
log=xc-$host.log
rm -rf "$tree"
rsync -a ccl/ "$tree/"
curl -sSfL -O https://github.com/Clozure/ccl/releases/download/v1.14/darwinx86.tar.gz
tar -xzf darwinx86.tar.gz -C "$tree"
ln -s darwin-x86-headers64 "$tree/cross-darwin-x86-headers64"
"$(dirname "$0")/vm" "$host" ccl-bc "$tree" > "$log" 2>&1 <<'EOF' || true
(progn
  (load "ccl:tools;cvm-xcompile.lisp")
  (cvm-load-cross-setup "ccl:tools;xdarwinx8664.lisp")
  (let ((*warn-if-redefine-kernel* nil))
    (cross-compile-ccl :darwinx8664 t)
    (cross-xload-level-0 :darwinx8664 :force))
  (format t "~&VM-XC-DONE~%"))
EOF
tail -40 "$log"
# Whole-line match: a backtrace can quote the form's source.
grep -qx VM-XC-DONE "$log"
(cd "$tree/lisp-kernel/darwinx8664" && make)
cd "$tree"
echo '(save-application "dx86cl64.image")' | ./dx86cl64 --image-name x86-boot64.image --no-init --batch
./dx86cl64 -n -b -e '(progn (format t "~&XC-BOOT-OK ~a~%" (lisp-implementation-version)) (quit))' | tee boot.log
grep -q '^XC-BOOT-OK' boot.log
