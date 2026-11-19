#!/bin/bash
# usage: [BUNDLE_TARBALL=FILE] install-hosts.sh [--bare] HOST...   (HOST is sbcl or ccl)
# Installs the host lisps that ccl-vm runs on, plus Quicklisp with ccl-vm's dependencies, and
# unpacks the bc bundle (FILE, default ccl-bc.tar.gz, downloaded by the workflow).
# sbcl comes from Homebrew, on Linux too (GitHub's x86-64 ubuntu images have it, off the PATH;
# it gets installed where missing).
# --bare: only the lisps and an empty Quicklisp, as a new user has (no dependencies, no bundle).
# For the ccl host, installs the released ccl 1.14 for this machine (Intel or Apple Silicon) in ./ccl-host/ and records its path in
# CCL_HOST (for the later steps, via GITHUB_ENV).
set -euo pipefail

# The released ccl for this machine: Intel or Apple Silicon.
. "$(dirname "$0")/platform.sh"
bare=false
[ "${1:-}" = --bare ] && { bare=true; shift; }
CCL_HOST=$PWD/ccl-host/$CCL_KERNEL

for host in "$@"; do
  case $host in
    sbcl)
      if ! command -v brew > /dev/null && [ "$(uname -s)" = Linux ] && [ ! -x /home/linuxbrew/.linuxbrew/bin/brew ]; then
        # GitHub's arm64 ubuntu images have no Homebrew.
        NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
      fi
      if ! command -v brew > /dev/null && [ -x /home/linuxbrew/.linuxbrew/bin/brew ]; then
        eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
        [ -n "${GITHUB_PATH:-}" ] && echo /home/linuxbrew/.linuxbrew/bin >> "$GITHUB_PATH"
      fi
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
$bare && exit 0
# Download and compile ccl-vm and its dependencies once per host.
prologue="$(cd "$(dirname "$0")" && pwd)/vm-prologue.lisp"
for host in "$@"; do
  case $host in
    sbcl) sbcl --noinform --non-interactive --load "$prologue" ;;
    ccl)  "$CCL_HOST" -n -b -l "$prologue" -e '(quit)' ;;
  esac
done

tar -xzf "${BUNDLE_TARBALL:-ccl-bc.tar.gz}"
