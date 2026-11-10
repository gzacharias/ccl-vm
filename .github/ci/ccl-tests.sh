#!/bin/bash
# usage: ccl-tests.sh LISP
# Runs Clozure/ccl-tests (in ./ccl-tests, cloned if it isn't there) with the native ccl LISP
# (a kernel, with its image next to it).  Log: ccl-tests.log.
set -u
lisp=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
[ -d ccl-tests ] || git clone -q https://github.com/Clozure/ccl-tests.git ccl-tests
status=0
make -C ccl-tests CCL="$lisp" > ccl-tests.log 2>&1 || status=$?
tail -60 ccl-tests.log
exit $status
