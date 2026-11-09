#!/bin/bash
# usage: install-hosts.sh HOST...   (HOST is sbcl or ccl)
# Installs the host lisps that ccl-vm runs on, plus Quicklisp with ccl-vm's dependencies, and
# unpacks the bc bundle (ccl-bc.tar.gz, downloaded by the workflow) into ./ccl-bc/.
# For the ccl host, installs the released ccl 1.14 for this machine (Intel or Apple Silicon) in ./ccl-host/ and records its path in
# CCL_HOST (for the later steps, via GITHUB_ENV).
set -euo pipefail

# The released ccl for this machine: Intel or Apple Silicon.
case $(uname -m) in
  arm64) ccl_platform=darwinarm64 ccl_kernel=darm64cl ;;
  *)     ccl_platform=darwinx86   ccl_kernel=dx86cl64 ;;
esac
CCL_HOST=$PWD/ccl-host/$ccl_kernel

for host in "$@"; do
  case $host in
    sbcl)
      brew install sbcl
      sbcl --version ;;
    ccl)
      curl -sSfL -O https://github.com/Clozure/ccl/releases/download/v1.14/ccl-1.14-$ccl_platform.tar.gz
      mkdir -p ccl-host
      tar -xzf ccl-1.14-$ccl_platform.tar.gz -C ccl-host --strip-components 1
      echo "CCL_HOST=$CCL_HOST" >> "$GITHUB_ENV"
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
