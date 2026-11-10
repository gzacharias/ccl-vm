# Regression tests

Small tests for bugs that were fixed, so they stay fixed.  Each test runs natively (a ccl
build) and in ccl-vm on each host, and checks against its own expected values, so the VM needs no
native reference output.

Larger CCL behavior belongs in ccl-tests (`git/ccl-tests`, the `PR-*-test` branches for upstream
PRs); this directory is for what ccl-vm's CI should check on every bundle: VM behavior, and
bc-compiler fixes that the VM depends on.

## Running

    tests/regression/run.sh native CCL-DIR [TEST...]       # e.g. native ccl
    tests/regression/run.sh HOST BUNDLE-DIR [TEST...]      # e.g. sbcl ccl-bc, ccl gen1

HOST is a `.github/ci/vm` host (sbcl uses `$SBCL`, ccl uses `$CCL_HOST`).  With no TEST names it
runs every test.  It prints one `ok`/`FAIL`/`skip` line per test, writes each test's output to
`regression-LABEL/NAME.log` in the current directory (LABEL: `native` or `HOST-<bundle dir>`), and
exits 1 when any test fails.  CI: native.yml runs it on the native build, vm.yml on the bundle and on
generation 1 (when the workflow's `checks` input is true).

## Adding a test

A test is one file, `NAME.lisp` or `NAME.sh`.  Start it with a comment saying what it checks and
what it guards: the commit(s), issue or PR whose fix it tests.

**NAME.lisp** is fed to the CCL listener after `harness.lisp`.  Each check is

    (regression-check NAME FORM EXPECTED)

FORM and EXPECTED are evaluated and compared with EQUAL; an error in FORM is a failure, not the end
of the file.  Wrap literal operands in `(identity ...)` when the compiler could fold the operation.
The test passes when the file runs to the end with no failed check.  Expected values must hold
natively and on every VM host.

**NAME.sh** is for what Lisp code can't check from inside (exit status, command line): it is run
as `NAME.sh native CCL-DIR` or `NAME.sh HOST BUNDLE-DIR`, prints `REGRESSION PASS ...` /
`REGRESSION FAIL ...` lines, and exits 0 (passed), 77 (doesn't apply there), or anything else
(failed).  Use `../../.github/ci/vm` to run the VM.

## Tests

| test | checks | guards |
|---|---|---|
| floats | float negation, signed zeros, NaNs; host arithmetic errors are CCL conditions | ccl-vm f4e6e22, 2719ec2 (#2) |
| nan | double-float-nan is the same constant everywhere; define-constant with NaNs | ccl 989a3ce9, PR-define-constant |
| exit-status | the vm script exits with CCL's exit status (VM only) | ccl-vm 67ce367 |
| cloop-restart | CLOOP runs the listener again after CCL quits (VM only, sbcl and ccl hosts) | ccl-vm 1b99b1b |
