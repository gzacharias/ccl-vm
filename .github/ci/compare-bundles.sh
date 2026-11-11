#!/bin/bash
# usage: compare-bundles.sh REF-LABEL=REF LABEL=BUNDLE...
# Checks that every BUNDLE is byte-identical to REF.  Each is a bundle directory or a .tar.gz of
# one (unpacked into a temporary directory).  A missing BUNDLE counts as a failure.  Appends the
# differences to bundle-diff.txt.  With COMPARE_LEVEL=warning, differences are ::warning::
# annotations and don't make the exit status nonzero (a missing bundle still does).
set -u
level=${COMPARE_LEVEL:-error}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
# Prints the bundle directory for $1 (unpacking a tarball), or nothing if it doesn't exist.
# (Runs in a subshell, so each tarball gets its own mktemp directory.)
bundle_dir() {
  if [ -d "$1" ]; then echo "$1"
  elif [ -f "$1" ]; then
    local d; d=$(mktemp -d "$tmp/b.XXXX"); tar -xzf "$1" -C "$d"
    # The tarball holds one top-level directory.
    echo "$d/$(ls "$d" | head -1)"
  fi
}
count() { find "$1" -name '*.bc' | wc -l | tr -d ' '; }
ref_label=${1%%=*}
ref=$(bundle_dir "${1#*=}")
shift
[ -n "$ref" ] || { echo "::error::no $ref_label bundle (${1#*=})"; exit 1; }
echo "$ref_label: $(count "$ref") bc files"
status=0
for arg in "$@"; do
  label=${arg%%=*}
  dir=$(bundle_dir "${arg#*=}")
  if [ -z "$dir" ]; then
    echo "::error::no $label bundle (${arg#*=}: its job didn't get that far)"; status=1; continue
  fi
  if diff -r "$ref" "$dir" > "$tmp/diff"; then
    echo "$label: byte-identical to $ref_label ($(count "$dir") files)"
  else
    echo "::$level::$label differs from $ref_label ($(grep -c '^diff\|^Only in' "$tmp/diff") files)"
    { echo "=== $label vs $ref_label"; cat "$tmp/diff"; } >> bundle-diff.txt
    [ "$level" = error ] && status=1
  fi
done
[ -f bundle-diff.txt ] && head -200 bundle-diff.txt
exit $status
