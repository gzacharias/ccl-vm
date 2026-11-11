#!/bin/bash
# usage: vm-rebuild.sh HOST BUNDLE-DIR NAME
# Boots BUNDLE-DIR in ccl-vm on HOST, runs (rebuild-ccl :clean t) on a fresh copy of the ccl
# sources in ./ccl/, and collects the resulting .bc files into a new bundle, ./NAME/.
# Log: NAME.log.
# *gensym-counter* is bound around rebuild-ccl so the gensyms dumped in the .bc files get the same
# names in every run, and bundles rebuilt on different hosts can be compared byte for byte.  Only
# the tests need this (bc-compile-ccl does the same for the native bundle); rebuild-ccl doesn't.
set -u
host=$1 bundle=$2 name=$3
tree=$name-tree
rm -rf "$tree" "$name"
rsync -a ccl/ "$tree/"
"$(dirname "$0")/vm" "$host" "$bundle" "$tree" > "$name.log" 2>&1 <<EOF
(progn
  (let ((*gensym-counter* 0)) (rebuild-ccl :clean t))
  (ccl::collect-all-fasls "$PWD/$name/")
  (format t "~&VM-REBUILD-DONE~%"))
EOF
tail -40 "$name.log"
# Whole-line match: a backtrace can quote the form's source.
grep -qx VM-REBUILD-DONE "$name.log" || exit 1
count=$(find "$name" -name '*.bc' | wc -l | tr -d ' ')
echo "bc files in $name: $count"
[ "$count" -eq "$(find "$bundle" -name '*.bc' | wc -l | tr -d ' ')" ] || { echo "::error::$name has $count bc files, $bundle has a different number"; exit 1; }
