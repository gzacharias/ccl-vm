(defpackage :ccl-vm
  (:use :common-lisp))

(in-package :cl-user)

(require'quicklisp)
(unless (find-package "CFFI")
  (ql:quickload 'cffi))

;; Yes, I'm supposed to use asdf, but it's so damn inflexible for development.

(let* ((path (or *load-pathname*
                #+allegro excl:*source-pathname*
                #+lispworks dspec:*source-pathname*
                #+sbcl (or *compile-file-truename* *load-truename*)
                #+ccl ccl:*loading-file-source-file*
                #+abcl (extensions:source-pathname)))
       (dir (make-pathname :name nil :type nil :defaults path)))
  (setf (logical-pathname-translations "cvm")
        `((#P"**;*.*" ,(merge-pathnames "**/*.*" (truename dir))))))

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
  (ensure-directories-exist "cvm:fasls;")
  (with-compilation-unit ()
    (loop for file in *ccl-vm-files*
      do (compile-file file
                       :output-file (make-pathname :name (pathname-name file) :defaults "cvm:fasls;")
                       :verbose verbose
                       :load t))))

(load-cvm)

(defun ccl::h (val)
  (format t "#x~x" val)
  val)

(defun ccl::show-lfun-bits (lfbits)
  (loop with prefix = ""
    for (flag bit) in '(("nonnullenv" 0)
                        ("keys" 1)
                        ;("numopt (byte 5 2))
                        ("restv" 7)
                        ;("numreq (byte 6 8))
                        ("optinit" 14)
                        ("rest" 15)
                        ("aok" 16)
                        ;("numinh (byte 6 17))
                        ("info" 23)
                        ("trampoline" 24)
                        ("code-coverage" 25)
                        ;; ("cm" 26)         ; combined-method SAME AS NEXTMETH
                        ("nextmeth" 26)
                        ("gfn" 27)
                        ("nextmeth-with-args" 27)
                        ("method" 28)
                        ("noname" 29))
    do (when (logbitp bit lfbits)
         (when (and (equal flag "nextmeth")
                    (not (logbitp ccl::$lfbits-method-bit lfbits)))
           (setq flag "cm"))
         (format t "~a~a" prefix flag)
         (setq prefix " "))))

(defmacro ccl::dfunc (sym)
  (when (ccl::quoted-form-p sym) (setq sym (cadr sym)))
  `(pprint (fourth (ccl-vm::ccl-function-bslambda (ccl-vm::sym-func (ccl-vm::ccl ',sym))))))

(import '(ccl::show-lfun-bits ccl::h ccl::dfunc) :ccl-vm)

