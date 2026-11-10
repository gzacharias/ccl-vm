#!/bin/bash
# usage: vm-cross-compile.sh HOST [TARGET]
# Boots ./ccl-bc in ccl-vm on HOST and cross-compiles a native ccl for TARGET (default
# darwinx8664) from a copy of the sources in ./ccl/, in ./xtree-HOST (darwinx8664) or
# ./xtree-HOST-TARGET.  If this machine can run TARGET, also checks that the result boots in the
# stock 1.14 kernel.  Logs: xc-HOST-TARGET.log, the tree's boot.log.
set -eu
here=$(dirname "$0")
. "$here/platform.sh"
host=$1 target=${2:-darwinx8664}
tree=xtree-$host
[ "$target" = darwinx8664 ] || tree=$tree-$target
rm -rf "$tree"
rsync -a ccl/ "$tree/"
"$here/cross-compile.sh" "$host" "$target" "$tree"
case $CCL_PLATFORM-$target in
  darwinx86-darwinx8664|darwinarm64-darwinarm64) "$here/boot-xc.sh" "$target" "$tree" ;;
  *) echo "(not booting: this machine is $CCL_PLATFORM)" ;;
esac
