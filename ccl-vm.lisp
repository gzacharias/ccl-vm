(in-package :cl-user)

;; Eventually use asdf I suppose, but it's too painful to use while debugging.

(let* ((path (or #+allegro excl:*source-pathname*
                 #+lispworks dspec:*source-pathname*
                 #+sbcl (let* ((location (sb-c:source-location))
                               (namestring (and location (sb-c:definition-source-location-namestring location))))
                          (and namestring (pathname namestring)))
                 #+ccl ccl:*loading-file-source-file*
                 #+abcl (extensions:source-pathname)
                 *load-pathname*))
       (dir (truename (make-pathname :name nil :type nil :version nil :defaults path))))
  ;; sbcl sure goes out if its way to make logical pathnames hard to use! The host has to be defined in order to
  ;; (make-pathname :host), and the host is required in the pathname given to logical-pathname-translations!
  (setf (logical-pathname-translations "cvm") (ignore-errors (logical-pathname-translations "cvm")))
  (setf (logical-pathname-translations "cvm")
        `((,(make-pathname :host "cvm" :directory '(:absolute :wild-inferiors) :name :wild :type :wild :version :wild)
           ,(make-pathname :directory (append (pathname-directory dir) '(:wild-inferiors))
                           :name :wild :type :wild :version :wild :defaults dir)))))

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

(defun load-ccl-vm (&key (verbose t))
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
  (load-ccl-vm))

#+ccl (progn
        (ccl::set-pprint-dispatch+ '(cons (member $bc-let*)) #'ccl::let-print '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-progv)) #'ccl::defvar-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-progn)) #'ccl::progn-print '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-block)) #'ccl::block-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-funcall)) #'ccl::block-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-if)) #'ccl::block-like '(0) ccl::*IPD*)
        )

