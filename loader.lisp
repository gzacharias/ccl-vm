(in-package :ccl-vm)

;; Maybe should load nfasload and just comment out %fasload?  I think there is a mechanism for
;; different fasload backends already!

;;;;; For testing only
(import 'ccl::test-load :ccl-vm)
(import 'ccl::test-vm :ccl-vm)
(defun ccl::test-load ()
  (cl-user::load-cvm) ;; load virtual machine - basic packages, functions.
  (load-cvmsrcs "CCL:")) ;; now load ccl into it.

(defvar *CCL-DIRECTORY*)

(defparameter *loading-ccl* nil)

;;; *** TODO: make disassemble do a pprint of the bslambda! or the bslambda-lambda

;;; 5 mins
(defun load-cvmsrcs (&optional (ccl-directory "CCL:"))
  (let ((*loading-ccl* t))
    (cvm-load-level-0 ccl-directory)
    ;; Ok, so this sets toplevel function at the end then throws to toplevel...
    ;; The toplevel func basically calls #'toplevel-loop
    (catch (ccl-symbol :toplevel)
      (lap-%fasload (sym-value (ccl-symbol '*xload-startup-file*)))))
  (format t "~&CCL-VM LOADED, Should run ~s" *ccl-toplevel-func*))

;; Build things up to the point where in the bootstrapping version, the heap image
;; has been loaded and all the initializations in %toplevel-function% in nfasload
;; have been executed up.
(defun cvm-load-level-0 (ccl-directory)
  (setq *CCL-DIRECTORY* (truename ccl-directory)) ;; VM needs this.
  (let* ((files (sort (directory (merge-pathnames "cvmsrcs/level-0/*.cvmsrc" *ccl-directory*))
                      #'string-lessp :key #'pathname-name))
         (calls (loop for file in files
                  unless (string-equal (pathname-name file) "nfasload")
                  nconc (let ((*deferred-level-0-calls* (list t)))
                          (cvmload file)
                          (loop for call in (cdr (nreverse *deferred-level-0-calls*))
                            collect (list file call))))))
    ;; Some stuff xfasload inits at image-build time
    ;; Most of this could be done before loading level-0, once packages exist.
    (%defvar (ccl-symbol '*package*) () 'variable *ccl-pkg*)
    (%defvar (ccl-symbol '*ccl-package*) () 'variable *ccl-pkg*)
    (%defvar (ccl '*common-lisp-package*) () 'variable *cl-pkg*)
    (%defconstant (ccl '%unbound-function%) *unbound-function*)
    (%defvar (ccl '*keyword-package*) () 'variable *keyword-pkg*)
    (%defvar (ccl'*gc-event-status-bits*) () 'variable 0)
    (%defvar (ccl '%toplevel-catch%) () 'variable (ccl :toplevel))
    ; %closure-code%, %macro-code%, %builtin-functions%
    ;; Macros sym-func is a vector #(<macro-code> fn)
    (%defvar (ccl '%macro-code%) () 'variable *macro-apply-code*)
    ;;(setf (xload-symbol-value (xload-copy-symbol '*xload-cold-load-documentation*))
    ;;      (xload-save-list (setq *xload-cold-load-documentation*
    ;;                             (nreverse *xload-cold-load-documentation*))))

    ;; default to unshared hash tables, lock-free-puthash seems to get an infinite loop *** TODO **** TRACK THIS DOWN
    ;;  Have to do this before %documentation is initialized, in level-0!
    (setf (sym-value (ccl '*shared-hash-table-default*)) nil)
    (setf (sym-value (ccl '*current-process*)) 1234) ;; needed for non-shared hash tables.
    
    ;; The "cold load" stream.
    (loop for (file fn) in calls as index upfrom 1
      do (format t "~& Call #~s (from ~s) " index file)
      do (ccl-funcall fn))

    ;;;; TODO******* So this needs to somehow come in from the compiler, because that's who knowns where it puts it.
    (%defvar (ccl '*xload-startup-file*) () 'variable (ccl "level-1.cvmsrc"))
    (%defvar (ccl '*openmcl-svn-revision*) () 'variable nil) ;; (local-vc-revision) -- SO THIS NEEDS TO BE FROM COMPILE/XLOAD time again
    (%defvar (ccl '*optional-features*) () 'variable nil) ;(mapcar 'ccl-symbol CCL::*BUILD-TIME-OPTIONAL-FEATURES*)

    (unbootstrap-documentation)
    ;;(unbootstrap-packages)
    ;;(%fasload *xload-startup-file*))
    (format t "~&Level-0 loaded~%")))

    

;;  When running in the VM, cvmsrc files need to be recognized as fasl files,
;;  so our {%fasload} function can run and do the load using cvmload.  This is
;;  accomplished by loading the cvm backend into the the VM, which makes {fasl-file-p}
;;  be true for cvmsrc files.

(defun cvmload  (file)
  (assert (equal (pathname-type file) "cvmsrc"))
  ;; Should we compile then load?  Only worth if can avoid the compile!
  ;; Which means we need to figure out fasl file conventions in the lisp.
  ;; Worry about it later
  (loop
    (restart-case ;; remove this once debugged
        (return (let ((*loader-table* nil)
                      (*package* (find-package :ccl-vm))
                      (cur-pkg (%sym-value (ccl'*package*)))
                      (cur-rdtable (%sym-value (ccl'*readable*))))
                  (declare (special *loader-table*))
                  (unwind-protect
                      ;; We want to load this as a source file. There is no way to ensure that portably,
                      ;; but in practice any lisp would interpret a random text file as source, except for
                      ;; this little weirdness in CCL that we introduced...
                      (let (#+ccl(ccl::*known-backends* (remove (pathname-type file) ccl::*known-backends*
                                                                :key (lambda (b)
                                                                       (pathname-type (ccl::backend-target-fasl-pathname b)))
                                                                :test 'equal)))
                        (load file))
                    (%set-sym-value (ccl'*package*) cur-pkg)
                    (%set-sym-value (ccl'*readable*) cur-rdtable) cur-rdtable)))
      (retry-load () :report (lambda (s) (format s "CVMLOAD ~s again" file))))))


;; a CVMSRC file is a bunch of toplevel calls to these $fasl functions.  The arguments
;; (once evaluated in the host lisp) are BSEVAL expressions, can then be BSEVAL'ed to yield
;; various native objects, or effect sideffects in the VM...

(defvar *fasl-trace* nil)
(defmacro fasl-trace (&rest format-args)
  `(when *fasl-trace*
     (let ((*print-pretty* t)
           (*print-circle* nil))
       (fresh-line *trace-output*)
       (format *trace-output* ,@format-args))))


(defun $fasl-init (num-imms)
  (declare (special *loader-table*))
  (assert (boundp '*loader-table*))
  (assert (null *loader-table*))
  (setq *loader-table* (make-array num-imms)))

(defun $fasl-set-package (str)
  (fasl-trace "~s ~s" '$fasl-set-package str)
  (check-type str ccl-simple-string)
  (let ((pkg (pkg-arg str)))
    ;;(assert (eq pkg *ccl-pkg*))
    (setf (sym-value (ccl '*package*)) pkg)))

(defun $fasl-defvar (sym &optional doc)
  (fasl-trace "~s ~s ~s" '$fasl-defvar sym doc)
  (%defvar sym doc 'variable))

(defun $fasl-defparameter (sym val doc)
  (fasl-trace "~s ~s ~s ~s" '$fasl-defparameter sym val doc)
  (%defvar sym doc 'variable)
  (setf (sym-value sym) val))

(defun $fasl-defvar-init (sym val doc)
  (fasl-trace "~s ~s ~s ~s" '$fasl-defvar-init sym val doc)
  (%defvar sym doc 'defvar)
  (unless (sym-boundp sym)
    (setf (sym-value sym) val)))


;; boostrapping until L1-utils
(defun $fasl-defconstant (sym val doc)
  (fasl-trace "~s ~s ~s ~s" '$fasl-constant sym val doc)
  ;; once bootstrapped, this will check for redefinition.  Not here.
  (%defconstant sym val doc))

(defun $fasl-defun (fn &optional doc)
  (fasl-trace "~s ~s ~s" '$fasl-defun fn doc)
  (%defun fn doc))

(defun $fasl-funcall (sym-or-fn)
  (fasl-trace "~s ~s" '$fasl-funcall sym-or-fn)
  (let ((fn (ensure-func sym-or-fn)))
    (if *deferred-level-0-calls*
      (push fn *deferred-level-0-calls*)
      (ccl-funcall fn))))

(defun $fasl-defmacro (fn doc)
  (fasl-trace "~s ~s ~s" '$fasl-defmacro fn doc)
  (check-type fn ccl-function)
  (let ((sym (ccl-function-name fn)))
    (check-type sym ccl-symbol)
    (record-debug-info sym doc 'function)
    (ccl-set-macro-function sym fn)))

(defun $fs-ref (index)
  (declare (special *loader-table*))
  (svref *loader-table* index))

(defun $fs-set (index value)
  (declare (special *loader-table*))
  (setf (svref *loader-table* index) value))

(defun $fs-unbound-marker () *unbound-marker*)
(defun $fs-slot-unbound-marker () *slot-unbound-marker*)
(defun $fs-illegal-marker () *illegal-marker*)

(defun $fs-package (name)
  (fasl-trace "   ~s ~s" '$fs-package name)
  (check-type name ccl-simple-string)
  (pkg-arg name))

(defun $fs-symbol (name pkg)
  (fasl-trace "   ~s ~s ~s" '$fs-symbol name pkg)
  (if (null pkg)
    (make-ccl-symvector name)
    (find-or-make-sym name pkg)))

(defun $fs-string (string)
  (check-type string string)
  (ccl-string string))

(defun $fs-make-uvector (type-key size)
  (fasl-trace "   ~s ~s ~s" '$fs-make-uvector type-key size)
  (check-type size fixnum)
  (alloc-uvector size (typekey-subtag type-key)))

(defun $fs-init-bslambda (bslambda)
  ;; We $BS-QUOTED the name and the keywords so as do get the fasdumper to do the right thing,
  ;; but don't want to have to always eval them.
  (flet ((unquot (thing)
           (if (and (consp thing) (consp (cdr thing)) (null (cddr thing))
                    (eq (car thing) '$bs-quote)
                    ;(typep (cadr thing) 'ccl-symbol)
                    )
             (cadr thing)
             (error "Expected a quoted object not ~s" thing))))
    (destructuring-bind (name (inh req opt rest keys bits) body num) (cdr bslambda)
      (declare (ignore inh req opt rest bits body num))
      (setf (cadr bslambda) (unquot name))
      (loop for info in (cdr keys)
        do (destructuring-bind (key var init supp) info
             (declare (ignore var init supp))
             (setf (car info) (unquot key)))))
    bslambda))
                   

(defun $fs-cons-function ()
  (fasl-trace "   ~s" '$fs-cons-function)
  (cons-ccl-function))

(defun $fs-init-function (fn bslambda)
  (let ((*print-length* 3) (*print-level* 3))
  (fasl-trace "   ~s ~s ~s" '$fs-init-function fn bslambda))
  (init-ccl-function fn bslambda))

(defun $fs-init-uvector (uvec &rest values)
  (fasl-trace "   ~s ~s ~s" '$fs-init-uvector uvec values)
  (assert (eq (uvsize uvec) (length values)))
  (loop for val in values as index upfrom 0
    do (setf (uvref uvec index) (ccl val)))
  uvec)

(defun $fs-make-array (native-key dims-list)
  (fasl-trace "   ~s ~s ~s" '$fs-make-array native-key dims-list)
  (break "NIY")
  'make-array-not-implemented-yet)

(defun $fs-init-array (arr &rest row-major-values)
  (fasl-trace "   ~s ~s ~s" '$fs-init-array arr row-major-values)
  (break "NIY")
  'init-array-not-implemented-yet)

(defun $fs-eval (expr)
  (fasl-trace "   ~s ~s" '$fs-eval expr)
  (when *deferred-level-0-calls*
    (error "$fs-eval in level-0 ~s" expr))
  (labels ((simple-eval (arg)
           (cond ((typep arg 'ccl-symvector) (sym-value arg))
                 ((atom arg) arg)
                 ((eq (car arg) (ccl'quote))
                  (assert (eql (length arg) 2))
                  (cadr arg))
                 ((typep (car arg) 'ccl-symvector)
                  ;; this will err out on macros or special forms
                  (apply-in-environment nil (car arg) (mapcar #'simple-eval (cdr arg))))
                 (t (error "Don't know how to eval ~s" expr)))))
    (simple-eval expr)))

;; like $fasl-funcall but for value, it's used in load-time values.
;;; I BELIEVE *ALL* calls to this a find-class-cell, maybe its worth breaking out,
;;; even just to call out to ccl.
;;; OR conversely, do we really need $fs-istruct-cell?  can we call something in ccl?
(defun $fs-funcall (fn)
  (fasl-trace "   ~s ~s" '$fs-funcall fn)
  ;(FORMAT *trace-OUTPUT* "~&$FS-FUNCALL ~s" (ccl-function-bslambda fn))
  (when *deferred-level-0-calls*
    (error "$fs-funcall in level-0 ~s" fn))
  (ccl-funcall fn))

(defun $fs-istruct-cell (sym)
  (fasl-trace "   ~s ~s" '$fs-istruct-cell sym)
  (check-type sym ccl-symbol)
  (register-istruct-cell sym))
