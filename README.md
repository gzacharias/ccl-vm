# ccl-vm

A virtual machine, written in portable Common Lisp, that can run Clozure CL inside another Lisp.

ccl-vm models the CCL runtime and can boot up CCL from ".bc" files generated from the CCL source code.  The .bc files are produced the [bc-compiler fork of ccl](https://github.com/gzacharias/ccl/tree/bc-compiler).

The .bc files are text files containing lisp forms, and can be checked into source control.

## Requirements

* A host Common Lisp running on macOS. Tested with ccl (1.12.2 and 1.13) and sbcl.
* Quicklisp is assumed to be installed in `~/quicklisp/`
* The .bc files bundle:  you can either download a prebuilt bundle from this repository's
[Releases](https://github.com/gzacharias/ccl-vm/releases), or build one yourself by following
 the instructions in [bc-compiler README](https://github.com/gzacharias/ccl/blob/bc-compiler/README.md).

## Running CCL in the VM

You can create a bc bundle yourself using the [bc-compiler](https://github.com/gzacharias/ccl/blob/bc-compiler/README.md),
or you can download a pre-built bc bundle from this repository:

```
curl -L -O https://github.com/gzacharias/ccl-vm/releases/latest/download/ccl-bc.tar.gz
tar -xzf ccl-bc.tar.gz        # makes ./ccl-bc/
```

Then, in your Lisp:
```
(load #P"/path/to/ccl-vm/ccl-vm.lisp")               ; load ccl-vm.
(ccl-vm:load-ccl #P"/path/to/ccl-bc/")               ; loads the bc bundle into the vm
(ccl-vm:cloop)                                       ; start the ccl listener in the vm
```

You should get CCL's prompt:

```
ccl? (+ 1 2)
3
ccl? (defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
FACT
ccl? (fact 20)
2432902008176640000
```

Functions are translated into host Lisp code the first time they are called, and compiled by the host's
compiler, so the first use of anything is slower than the second.

If you plan to recompile ccl, or reference the ccl sources in any anyway, you need to point the vm's "ccl:" logical host
to where the ccl sources are (by default it gets set to the directory with the bc files).  You can do so either by
setting the `CCL_VM_DEFAULT_DIRECTORY` shell variable to the source dir, or passing it as an argument to`load-ccl`:
```
(ccl-vm:load-ccl "/path/to/ccl-bc/" :ccl-directory "/path/to/sources/")
```
If you are running the vm inside ccl, and want to use the same sources, you can just do
```
(ccl-vm:load-ccl "ccl:ccl-bc;" :ccl-directory "ccl:")
```


## State of the project

Currently the virtual machine can boot up and run the CCL REPL, though only with limited error handling.  It can run (rebuild-ccl :clean t) to produce a new set of .bc files, boot from it and run rebuild-ccl again and produce the same result.  There are however a lot of warnings, and functionality outside what's used by rebuild-ccl hasn't been tested.

The next milestone for this project would be cross compiling to another backend (e.g. x8664) from the VM.

The foreign function support is kludgy and limited to darwin, so it can only run on darwin hosts.

The VM is single-threaded.  The primary goal of this project is to be able to run the CCL compiler in another lisp, so threads are not a priority.

On my machine, rebuild-ccl takes about 80 minutes in ccl, about 7 minutes in sbcl.


## How it works

The bc-compiler turns CCL's source into sexps such as `($BC-IF ($BC-LREF 0) ...)`. ccl-vm implements those operators as Lisp macros in such a way that loading the .bc files translates the sexps into the host lisp's lambdas, which get compiled lazily. Primitives that CCL implements in assembly or in its C kernel are simulated by the VM, and operating system calls go through CFFI.


## License

Apache License 2.0, the same as Clozure CL. See [LICENSE](LICENSE).
