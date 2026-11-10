#!/bin/bash
# usage: compare-xc.sh TARGET NATIVE-TREE VM-TREE
# Compares the VM's TARGET cross-compile with the native one: a different set of fasls fails.
# Known difference (gzacharias/ccl-vm#13): native fasls use the "-special" symbol opcodes for
# symbols the compiling image happened to bind, and a few fasls differ otherwise, so differing
# contents (fasls, boot image) are only reported as warnings.
set -u
. "$(dirname "$0")/platform.sh"
target=$1 native=$2 vm=$3
target_info "$target" || exit 1
status=0
if cmp -s "$native/$T_BOOT_IMAGE" "$vm/$T_BOOT_IMAGE"; then echo "boot images are byte-identical"
else echo "::warning::the $target boot images differ (known, ccl-vm#13)"; fi
list() { (cd "$1" && find . -name "*.$T_FASL" | sort); }
list "$native" > native.list
list "$vm" > vm.list
diff native.list vm.list > list.diff || { echo "::error::different sets of $T_FASL files"; cat list.diff; status=1; }
ndiff=0
while read -r f; do cmp -s "$native/$f" "$vm/$f" || { echo "differs: $f"; ndiff=$((ndiff+1)); }; done < native.list
echo "$T_FASL files: $(wc -l < native.list | tr -d ' '), differing: $ndiff"
[ $ndiff = 0 ] || echo "::warning::$ndiff of $(wc -l < native.list | tr -d ' ') $T_FASL files differ (known, ccl-vm#13)"
exit $status
