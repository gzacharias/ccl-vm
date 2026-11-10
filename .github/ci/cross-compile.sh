#!/bin/bash
# usage: cross-compile.sh HOST TARGET TREE
# Cross-compiles a native ccl for TARGET (darwinx8664, darwinarm64) from the sources in TREE,
# leaving the TARGET fasls and boot image in TREE.  HOST is native (TREE is itself a native
# build for this machine, which does the cross-compile) or a VM host (sbcl, ccl: the bundle in
# ./ccl-bc, or $BUNDLE, booted in ccl-vm on HOST, with TREE as its "ccl:").  Unpacks TARGET's
# interface headers from the 1.14 release into TREE if they aren't there.  Log: xc-HOST-TARGET.log.
set -u
here=$(cd "$(dirname "$0")" && pwd)
. "$here/platform.sh"
host=$1 target=$2 tree=$3
target_info "$target" || exit 1
log=$PWD/xc-$host-$target.log
[ -d "$tree/$T_HEADERS" ] || tar -xzf "$(fetch_release $T_RELEASE)" -C "$tree" "$T_HEADERS" || exit 1
# xdarwinx8664.lisp looks for the headers under a cross- name, so they don't clash with the host's.
[ "$target" = darwinx8664 ] && [ ! -e "$tree/cross-$T_HEADERS" ] && ln -s "$T_HEADERS" "$tree/cross-$T_HEADERS"
forms="(progn
  (load \"ccl:tools;$T_XLOAD.lisp\")
  (let ((*warn-if-redefine-kernel* nil))
    (ccl::cross-compile-ccl :$target t)
    (ccl::cross-xload-level-0 :$target :force))
  (format t \"~&XC-DONE~%\"))"
case $host in
  native) (cd "$tree" && printf '%s\n(quit)\n' "$forms" | ./$CCL_KERNEL -n -b) > "$log" 2>&1 ;;
  *)      printf '%s\n' "$forms" | "$here/vm" "$host" "${BUNDLE:-ccl-bc}" "$tree" > "$log" 2>&1 ;;
esac
tail -40 "$log"
# Whole-line match: a backtrace can quote the form's source.
grep -qx XC-DONE "$log" || { echo "::error::the $target cross-compile ($host) didn't finish"; exit 1; }
ls -l "$tree/$T_BOOT_IMAGE" || exit 1
echo "$T_FASL files: $(find "$tree" -name "*.$T_FASL" | wc -l | tr -d ' ')"
