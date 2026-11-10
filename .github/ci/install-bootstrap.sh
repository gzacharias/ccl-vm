#!/bin/bash
# usage: install-bootstrap.sh [CCL-DIR]
# Unpacks the released ccl 1.14 binaries for this machine (kernel, image, headers) into CCL-DIR
# (default ./ccl, a checkout of the sources to build) and records the machine, toolchain and
# ccl versions in system.log.  Records the kernel's name in CCL_KERNEL for later workflow steps.
set -euo pipefail
. "$(dirname "$0")/platform.sh"
dir=${1:-ccl}
tar -xzf "$(fetch_release $CCL_PLATFORM.tar.gz)" -C "$dir"
github_env "CCL_KERNEL=$CCL_KERNEL"
{
  if [ "$(uname -s)" = Darwin ]; then
    sw_vers; uname -m; sysctl -n hw.ncpu machdep.cpu.brand_string
    clang --version | head -1
  else
    grep PRETTY_NAME /etc/os-release; uname -m; nproc
    gcc --version | head -1; m4 --version | head -1
  fi
  make --version | head -1
  git -C "$dir" log -1 --format='ccl %h %s' || true
  "$dir/$CCL_KERNEL" -n -b -e '(progn (format t "~&~a~%" (lisp-implementation-version)) (quit))'
} 2>&1 | tee system.log
