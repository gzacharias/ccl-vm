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
#+gz
(unless (find-package "METERING")
  (ql:quickload 'metering))


(defpackage :ccl-vm
  (:use :common-lisp)
  ;; (:import-from :split-sequence #:split-sequence)
  (:import-from :alexandria
                #:when-let
                #:when-let*
                #:if-let
                #:starts-with-subseq
                #:ends-with-subseq
                #:set-equal)
  #+gz (:import-from :monitor
                     #:monitor
                     #:monitor-all
                     #:unmonitor
                     #:reset-all-monitoring
                     #:report-monitoring)
)

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

;;;; Things to monitor:
;;; (a) (compile-cvm t) running in CCL - this is no problem, it's fast
;;; (b) (load-ccl) - this loads up all the BC files, runs some CCL-VM fns, and then also starts calling some BC functions.  It's medium slow.
;;; (c) ({compile-cvm}t) - running in the VM, this is deadly slow.
;;;   Monitoring vm precompiles all the functions, so not quite the same as unmonitored runs, but we could in theory precompile all the functions
;;;   and save an image after load-ccl.

#+gz (progn

(let ((encapsulation (ccl::compile-named-function (monitor::make-monitoring-encapsulation 3 nil))))
  (declare (type function encapsulation))
  (defun monitor-vm ()
    (flet ((do-syms (htab)
             (loop for sym across (gvector-data (car htab))
               as fn = (and (not (eql sym 0))
                            (let ((fn (sym-fboundp sym)))
                              (and (ccl-function-p fn)
                                   (consp (ccl-function-bclambda fn))
                                   fn)))
               when fn
               do (progn
                    (unless (ccl-function-native-fn fn)
                      (setf (ccl-function-native-fn fn)
                            (compile-native-function (ccl-function-name fn)
                                                     (bclambda-lambda (ccl-function-bclambda fn)))))
                    
                    (funcall encapsulation fn)))))
      (do-syms (uvref *ccl-pkg* pkg.itab))
      (do-syms (uvref *ccl-pkg* pkg.etab)))))

;; (monitored (ccl-funcall (ccl'compile-cvm) t) t)
(defmacro monitored (form &optional vm-too?) ;; run form with monitoring turned on for all ccl-vm and VM fns
  `(unwind-protect
       (progn
         (monitor-all :ccl-vm)
         (unmonitor ccl-vm::ccl-fn)
         (let ((ccl::*warn-if-redefine-kernel* nil))
           (monitor cl:eval))
         ;(monitor ccl:structure-typep)
         (when ,vm-too? (monitor-vm))
         (reset-all-monitoring)
         (format t "~&Calling with monitoring ~s" ',form)
         (time ,form))
     (report-monitoring :all :exclusive 1.0 :percent-time)
     (let ((ccl::*warn-if-redefine-kernel* nil))
       (unmonitor))))
;; (ccl::advise cvmload (monitored (:do-it)) :when :around :name :monitor-cvm) ;; to monitor each call to cvmload separately
;; (ccl::unadvise cvmload :when :around :name :monitor-cvm)
;; (load-ccl)

  ;; (monitor-in-func 'compile-file)
(defun monitor-in-func (sym) ;; turn on monitoring while executing sym-func of sym.  Like the advise above but for VM
  (let* ((fn (sym-func (ccl-symbol sym)))
         (native-fn (ccl-function-native-fn fn)))
    (unless native-fn
      (setq native-fn (compile-native-function (ccl-function-name fn)
                                               (bclambda-lambda (ccl-function-bclambda fn))))
      (setf (ccl-function-native-fn fn) native-fn))
    (setf (ccl-function-native-fn fn) (lambda (&rest args) (monitored (apply native-fn args) t)))))
 ;; (unmonitor-in-func 'compile-file)
(defun unmonitor-in-func (sym)
  (setf (ccl-function-native-fn (sym-func (ccl-symbol sym))) nil))

 (ccl::advise mon::monitoring-unencapsulate
                  (let ((name (car ccl::arglist)))
                    (if (ccl-function-p name)
                      (let ((finfo (mon::get-monitor-info name)))
                        (setf (ccl-function-native-fn name)
                              (if (and finfo
                                       (eq (ccl-function-native-fn name)
                                           (mon::metering-functions-new-definition finfo)))
                                (mon::metering-functions-old-definition finfo)
                                nil))
                        (setq mon::*monitored-functions* (remove name mon::*monitored-functions*)))
                      (:do-it)))
                  :when :around :name :monitor-cvm)
) ;; #+gz

#+ccl (progn
        (ccl::set-pprint-dispatch+ '(cons (member $bc-let*)) #'ccl::let-print '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-progv)) #'ccl::defvar-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-progn)) #'ccl::progn-print '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-block)) #'ccl::block-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-funcall)) #'ccl::block-like '(0) ccl::*IPD*)
        (ccl::set-pprint-dispatch+ '(cons (member $bc-if)) #'ccl::block-like '(0) ccl::*IPD*)
        )

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

