# ccl-vm

A virtual machine, written in portable Common Lisp, that can run Clozure CL inside another Lisp.

ccl-vm models the CCL runtime and can boot up CCL from ".bc" files generated from the CCL source code.  The .bc files are produced by the [bc-compiler fork of CCL](https://github.com/gzacharias/ccl/tree/bc-compiler).

The .bc files are text files containing lisp forms, and can be checked into source control.  They take the place of the image file used by native CCL.

## Requirements

* A host Common Lisp running on macOS. Tested with ccl (1.14) and sbcl (2.6)
* ASDF and Quicklisp
* The .bc files bundle:  you can either download a prebuilt bundle from this repository's
[Releases](https://github.com/gzacharias/ccl-vm/releases), or build one yourself by following
 the instructions in [bc-compiler README](https://github.com/gzacharias/ccl/blob/bc-compiler/README.md)

## Running CCL in the VM

You can create a bc bundle yourself using the [bc-compiler](https://github.com/gzacharias/ccl/blob/bc-compiler/README.md),
or you can download a pre-built bc bundle from this repository as follows:

```
curl -L -O https://github.com/gzacharias/ccl-vm/releases/latest/download/ccl-bc.tar.gz
tar -xzf ccl-bc.tar.gz        # makes ./ccl-bc/
```

Then, in your Lisp:
```
(load "~/quicklisp/setup.lisp")         ; or whatever you do to install quicklisp
;; Tell quicklisp about ccl-vm.  You can do so by putting a link to it in ~/common-lisp/
;; or ~/quicklisp/local-projects/, or add it to the ASDF registry at runtime like this:
(pushnew #P"/path/to/ccl-vm/" asdf:*central-registry* :test #'equal)
(ql:quickload "ccl-vm")                 ; load ccl-vm.
(ccl-vm:load-ccl #P"/path/to/ccl-bc/")  ; loads the bc bundle into the vm
(ccl-vm:cloop)                          ; start the ccl listener in the vm
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

If you plan to recompile CCL, or reference the CCL sources in any way, you need to point the vm's "ccl:" logical host
to where the CCL sources are (by default it gets set to the directory with the bc files).  You can do so either by
setting the `CCL_VM_DEFAULT_DIRECTORY` shell variable to the source dir, or passing it as an argument to `load-ccl`:
```
(ccl-vm:load-ccl "/path/to/ccl-bc/" :ccl-directory "/path/to/sources/")
```
If you are running the vm inside ccl, and want to use the same sources, you can just do
```
(ccl-vm:load-ccl "ccl:ccl-bc;" :ccl-directory "ccl:")
```

## Compiling CCL in the VM

CCL running in ccl-vm can recompile itself in the usual way:
```
ccl? (rebuild-ccl :clean t)
```
This will leave `.bc` files in the VM's "ccl:" directory, in all the places that fasl files get normally placed by `rebuild-ccl`.  To gather them into a new ccl-bc bundle, use this:

```
ccl? (ccl::collect-all-fasls "/path/to/new-ccl-bc/")
```

## Cross-compiling from the VM

CCL running in ccl-vm can cross-compile a native CCL.  So far this has been tested for darwinx8664 and darwinarm64: the VM, hosted in sbcl or ccl, compiles ccl's sources (the bc-compiler branch) and writes a bootstrapping image that the native kernel can boot from.

To do this, load ccl-vm with the bundle, pointing `ccl:` at a copy of the sources as described above.  You will need to get a copy of the headers directory for the target, which should be placed in "ccl:darwin-arm64-headers;" for the darwinarm64 target, and "ccl:cross-darwin-x86-headers64;" (note the extra **cross-** in the name) for the darwinx8664 target.  Then in the VM's `cloop`:

```
ccl? (load "ccl:tools;xdarwinarm64.lisp")
ccl? (let ((*warn-if-redefine-kernel* nil))
       (cross-compile-ccl :darwinarm64 t)
       (cross-xload-level-0 :darwinarm64 :force))
```
or

```
ccl? (load "ccl:tools;xdarwinx8664.lisp")
ccl? (let ((*warn-if-redefine-kernel* nil))
       (cross-compile-ccl :darwinx8664 t)
       (cross-xload-level-0 :darwinx8664 :force))
```

On GitHub's Intel macOS runners this takes about 10 minutes hosted in SBCL, about 50 in CCL.  It will produce a boot image (arm64-boot.image or x86-boot64.image, respectively) and all the fasl files in the ccl: directory, which the standard kernel from ccl 1.14 running on the target system can boot from, for example:
```
./dx86cl64 --image-name x86-boot64.image
(save-application "dx86cl64.image")
```

## State of the project

Currently the virtual machine, hosted in any combination of sbcl/ccl and arm64/x8664 running on MacOS, has reached the following milestones:

* It can boot up and run the CCL REPL, though only with limited error handling.

* It can run (rebuild-ccl :clean t) to produce a new set of .bc files, boot from it and run rebuild-ccl again and produce the same result.  It produces the same .bc files on any of the 4 supported host combinations.

* It can also cross-compile a native ccl: it builds a darwinx8664 (Intel macOS) or darwinarm64 (Apple Silicon) bootstrapping image, which the native kernel boots into a working CCL, as described above.

Functionality outside what is needed to compile or cross compile CCL hasn't been tested.

The foreign function support is kludgy and limited to darwin, so it can only build for darwin targets.

The VM is single-threaded.  The primary goal of this project is to be able to run the CCL compiler in another lisp, so threads are not a priority.

On my machine (Intel imac), rebuild-ccl takes about half an hour in CCL, about 6 minutes in SBCL.  On GitHub's Intel macOS runners it takes about an hour in CCL, about 10 minutes in SBCL, and cross-compiling takes about 50 minutes in CCL, about 10 in SBCL.


## How it works

The bc-compiler turns CCL's source into sexps such as `($BC-IF ($BC-LREF 0) ...)`. ccl-vm implements those operators as Lisp macros in such a way that loading the .bc files translates the sexps into the host lisp's lambdas, which get compiled lazily. Primitives that CCL implements in assembly or in its C kernel are simulated by the VM, and operating system calls are deferred to runtime and go through CFFI.


## License

Apache License 2.0, the same as Clozure CL. See [LICENSE](LICENSE).
