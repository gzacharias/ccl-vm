#!/bin/bash
# usage: boot-xc.sh TARGET TREE
# On a TARGET machine: boots the TARGET boot image that cross-compile.sh left in TREE in the stock
# 1.14 kernel (unpacked into TREE from the release), saves a full image and checks that it starts.
# Log: TREE/boot.log.
set -u
. "$(dirname "$0")/platform.sh"
target=$1 tree=$2
target_info "$target" || exit 1
tar -xzf "$(fetch_release $T_RELEASE)" -C "$tree" "$T_KERNEL" || exit 1
cd "$tree"
echo "(save-application \"$T_KERNEL.image\")" | ./$T_KERNEL --image-name $T_BOOT_IMAGE --no-init --batch > boot.log 2>&1
tail -40 boot.log
./$T_KERNEL -n -b -e '(progn (format t "~&XC-BOOT-OK ~a~%" (lisp-implementation-version)) (quit))' 2>&1 | tee -a boot.log
grep -q '^XC-BOOT-OK' boot.log || { echo "::error::the cross-compiled $target image doesn't start"; exit 1; }
