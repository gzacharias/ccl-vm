#!/bin/bash
# usage: install-hosts.sh HOST...   (HOST is sbcl or ccl)
# Installs the host lisps that ccl-vm runs on, plus Quicklisp with ccl-vm's dependencies, and
# unpacks the bc bundle (ccl-bc.tar.gz, downloaded by the workflow) into ./ccl-bc/.
# For the ccl host, installs the released ccl 1.14 for this machine (Intel or Apple Silicon) in ./ccl-host/ and records its path in
# CCL_HOST (for the later steps, via GITHUB_ENV).
set -euo pipefail

# The released ccl for this machine: Intel or Apple Silicon.
. "$(dirname "$0")/platform.sh"
CCL_HOST=$PWD/ccl-host/$CCL_KERNEL

for host in "$@"; do
  case $host in
    sbcl)
      brew install sbcl
      sbcl --version ;;
    ccl)
      mkdir -p ccl-host
      tar -xzf "$(fetch_release ccl-1.14-$CCL_PLATFORM.tar.gz)" -C ccl-host --strip-components 1
      github_env "CCL_HOST=$CCL_HOST"
      "$CCL_HOST" -n -b -e '(progn (format t "~&~a~%" (lisp-implementation-version)) (quit))' ;;
    *)
      echo "unknown host $host" >&2; exit 1 ;;
  esac
done

if [ ! -f ~/quicklisp/setup.lisp ]; then
  curl -sSfL -O https://beta.quicklisp.org/quicklisp.lisp
  case $1 in
    sbcl) sbcl --noinform --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' ;;
    ccl)  "$CCL_HOST" -n -b -l quicklisp.lisp -e '(quicklisp-quickstart:install)' -e '(quit)' ;;
  esac
fi
# Download and compile ccl-vm's dependencies once per host.
for host in "$@"; do
  case $host in
    sbcl) sbcl --noinform --non-interactive --load ~/quicklisp/setup.lisp --eval '(ql:quickload (list "cffi" "alexandria"))' ;;
    ccl)  "$CCL_HOST" -n -b -l ~/quicklisp/setup.lisp -e '(ql:quickload (list "cffi" "alexandria"))' -e '(quit)' ;;
  esac
done

tar -xzf ccl-bc.tar.gz
