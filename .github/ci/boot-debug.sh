#!/bin/bash
# usage: boot-debug.sh [CCL-DIR]
# After a failed rebuild: if the new boot image doesn't boot, rebuild-ccl only reports that it
# failed (it runs it in batch mode).  This boots it again outside batch mode, so a crash lands in
# the kernel debugger, which reads these commands from stdin: describe the exception, registers,
# lisp registers, backtrace, kill.  Log: boot-debug.log.
set -u
. "$(dirname "$0")/platform.sh"
dir=${1:-ccl}
cd "$dir"
image=$(ls *-boot*.image 2>/dev/null | head -1)
[ -n "$image" ] || { echo "::error::no boot image"; exit 1; }
echo "boot image: $image"
status=0
printf 'D\nR\nL\nB\nK\n' | ./$CCL_KERNEL --image-name "$image" --no-init > ../boot-debug.log 2>&1 || status=$?
cat ../boot-debug.log
echo "exit status $status"
