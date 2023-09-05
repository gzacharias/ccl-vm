(in-package :cl-user)

;; Eventually use asdf I suppose, but it's too painful to use while debugging.

(let* ((path (or *load-pathname*
                #+allegro excl:*source-pathname*
                #+lispworks dspec:*source-pathname*
                #+sbcl (or *compile-file-truename* *load-truename*)
                #+ccl ccl:*loading-file-source-file*
                #+abcl (extensions:source-pathname)))
       (dir (truename (make-pathname :name nil :type nil :version nil :defaults path))))
  ;; sbcl sure goes out if its way to make logical pathnames hard to use! The host has to be defined in order to
  ;; (make-pathname :host), and the host is required in the pathname given to logical-pathname-translations!
  (setf (logical-pathname-translations "cvm") (ignore-errors (logical-pathname-translations "cvm")))
  (setf (logical-pathname-translations "cvm")
        `((,(make-pathname :host "cvm" :directory '(:absolute :wild-inferiors) :name :wild :type :wild :version :wild)
           ,(make-pathname :name :wild :type :wild :version :wild :defaults dir)))))

(require "QUICKLISP" "~/quicklisp/setup.lisp")
(unless (find-package "CFFI")
  (ql:quickload 'cffi))
(unless (find-package "ALEXANDRIA")
  (ql:quickload 'alexandria))


(defpackage :ccl-vm
  (:use :common-lisp)
  ;; (:import-from :split-sequence #:split-sequence)
  (:import-from :alexandria
                #:when-let
                #:when-let*
                #:if-let
                #:starts-with-subseq
                #:ends-with-subseq
                #:set-equal))

(in-package :ccl-vm)

(defparameter *ccl-vm-files*
  '("cvm:ccl-vm.lisp"
    "cvm:defs.lisp"
    "cvm:types.lisp"
    "cvm:syms.lisp"
    "cvm:funcs.lisp"
    "cvm:bceval.lisp"
    "cvm:runtime.lisp"
    "cvm:loader.lisp"
    ))

(defvar *loading-ccl* nil)

(defun load-cvm (&key (verbose t))
  (let ((*loading-ccl* t))
    (ensure-directories-exist "cvm:fasls;")
    (with-compilation-unit ()
      (loop for file in *ccl-vm-files*
        ;; Compile them so with-compilation-unit can do its thing...
        as fasl = (compile-file file
                                :output-file (make-pathname :name (pathname-name file) :defaults "cvm:fasls;")
                                :verbose (and (not (eq verbose :load)) verbose))
        when (null fasl) do (error "Compile of ~s failed" file)
        do (load fasl :verbose (and (not (eq verbose :compile)) verbose))))))

(unless *loading-ccl*
  (load-cvm))

(defun edit-cvm () (map nil #'ed *ccl-vm-files*))

(import '(load-cvm edit-cvm) :cl-user)
#+ccl (import '(load-cvm edit-cvm) :ccl)


#+ccl (progn
        (ccl::set-pprint-dispatch+ '(cons (member $bc-let*)) #'ccl::let-print '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-progv)) #'ccl::defvar-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-progn)) #'ccl::progn-print '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-block)) #'ccl::block-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-funcall)) #'ccl::block-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-if)) #'ccl::block-like '(0) ccl::*IPD*)
        )

#+gz (defun find-inits ()
       (labels ((is-ok (exp)
                  (and (consp exp)
                       (symbolp (car exp))
                       (or (member (car exp) '(ccl-vm::defun-inline
                                                  ccl-vm::def-uvector-print-text
                                                  ccl-vm::def-uvector-subtype
                                                ccl-vm::defbceval ccl-vm::deflapfunction ccl-vm::deflapfunction
                                                ccl-vm::def-external-call
                                                ccl-vm::define-subtags
                                                ccl-vm::def-num-op ccl-vm::defvar-typed
                                                #+hemlock hemlock-interface:defindent
                                                defstruct cffi:defctype cffi:defcstruct cffi:defcfun cffi:defcvar
                                                defun defmacro defmethod define-symbol-macro in-package defpackage deftype defconstant))
                           (and (member (car exp) '(defvar defparameter))
                                (ccl::constantp (caddr exp)))
                           (and (eq (car exp) 'eval-when)
                                (or (null (intersection '(load :load-toplevel) (cadr exp)))
                                    (every #'is-ok (cddr exp))))))))

         (let ((*Package* (find-package :ccl-vm)))
           (loop for file in *ccl-vm-files* as new-file = t then t
             do (with-open-file (f file)
                  (loop for exp = (read f nil f) until (eql exp f)
                    do (unless (is-ok exp)
                         (when (shiftf new-file nil) (format t "~&File: ~s~%" file))
                         (format t "~&~s" exp))))))))

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

