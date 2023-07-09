(defpackage :ccl-vm
  (:use :common-lisp))

(require'quicklisp)
(unless (find-package "CFFI")
  (ql:quickload 'cffi))

;; Yes, I'm supposed to use asdf, but it's so damn inflexible for development.

(let ((path (or *load-pathname*
                #+allegro excl:*source-pathname*
                #+lispworks dspec:*source-pathname*
                #+sbcl (or *compile-file-truename* *load-truename*)
                #+ccl ccl:*loading-file-source-file*
                #+abcl (extensions:source-pathname))))
  (setf (logical-pathname-translations "cvm")
        `((#P"**;*.*" ,(merge-pathnames "**/*.*" (truename path))))))

(defparameter *ccl-vm-files*
  '("cvm:defs.lisp"
    "cvm:types.lisp"
    "cvm:syms.lisp"
    "cvm:funcs.lisp"
    "cvm:loader.lisp"
    "cvm:vm-bseval.lisp"
    "cvm:runtime.lisp"
    ))

(defun load-cvm (&key (verbose t))
  (with-compilation-unit ()
    (loop for file in *ccl-vm-files*
      do (compile-file file :verbose verbose :load t))))

(load-cvm)

;;;;; For testing only
#+ccl
(defun ccl::test-load ()
  ;; Don't really understand the intended way of doing this.  Any attempt to
  ;; use a target ends up calling FIND-BACKEND, but there is no cvm backend until
  ;; these files are loaded, so just do it.
  ;(load "ccl:compiler;cvm;cvm-arch.lisp")
  ;(load "ccl:compiler;cvm;cvm-backend.lisp")
  (load-cvm)
  (let ((files (sort (directory "ccl:level-0;cvmfasls;*.cvmfsl") #'string-lessp :key #'pathname-name)))
    (ccl-vm::cvmload-level-0 files)))

