(defpackage :ccl-vm
  (:use :common-lisp))

(in-package :cl-user)

(require'quicklisp "~/quicklisp/setup.lisp")
(unless (find-package "CFFI")
  (ql:quickload 'cffi))

(let* ((path (or *load-pathname*
                #+allegro excl:*source-pathname*
                #+lispworks dspec:*source-pathname*
                #+sbcl (or *compile-file-truename* *load-truename*)
                #+ccl ccl:*loading-file-source-file*
                #+abcl (extensions:source-pathname)))
       (dir (truename (make-pathname :name nil :type nil :version nil :defaults path))))
  ;; sbcl sure goes out if its way to make logical pathnames hard to use! The host has to be defined in order to
  ;; (make-pathname :host), and the host is required in the pathname given to logical-pathname-translations!
  (setf (logical-pathname-translations "cvm") nil)
  (setf (logical-pathname-translations "cvm")
        `((,(make-pathname :host "cvm" :directory '(:absolute :wild-inferiors) :name :wild :type :wild :version :wild)
           ,(make-pathname :name :wild :type :wild :version :wild :defaults dir)))))

(defparameter *ccl-vm-files*
  '("cvm:defs.lisp"
    "cvm:types.lisp"
    "cvm:syms.lisp"
    "cvm:funcs.lisp"
    "cvm:loader.lisp"
    "cvm:bceval.lisp"
    "cvm:runtime.lisp"
    ))

(defun load-cvm (&key (verbose t))
  (ensure-directories-exist "cvm:fasls;")
  (with-compilation-unit ()
    (loop for file in *ccl-vm-files*
      ;; Compile them so with-compilation-unit can do its thing...
      as fasl = (compile-file file
                              :output-file (make-pathname :name (pathname-name file) :defaults "cvm:fasls;")
                              :verbose verbose)
      when (null fasl) do (error "Compile of ~s failed" file)
      do (load fasl))))

(load-cvm)

(defun edit-cvm () (map nil #'ed *ccl-vm-files*))

(import '(load-cvm edit-cvm) :ccl-vm)

#+ccl
(defun ccl::h (val)
  (format t "#x~x" val)
  val)

#+ccl
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

#+ccl
(defmacro ccl::dfunc (sym)
  (when (ccl::quoted-form-p sym) (setq sym (cadr sym)))
  `(ppfun (ccl-vm::ccl ',sym)))

#+ccl
(defun ppfun (func-or-sym)
  (let* ((func (ccl-vm::ensure-func (ccl-vm::ccl func-or-sym)))
         (ccl::*print-right-margin* 150)
         (bclambda (ccl-vm::ccl-function-bclambda func)))
    (format t "~&~s ~s ~s" (first bclambda) (second bclambda) (third bclambda))
    (pprint (fourth bclambda))))

#+ccl
(import '(ccl::show-lfun-bits ccl::h ccl::dfunc) :ccl-vm)

