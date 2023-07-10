(in-package :ccl-vm)

;;;; *** TODO: there are too many forward references in level-0. 
(defvar *deferred-level-0-calls* nil)

;; Don't load nfasload!  We don't plan to use it, we just want to be able to
;;  use the compiler and we'll be using our loader.

;; Then can replace the stuff in there that's used elsewhere with LAP, and
;;  keep packages fast.
#|  Stuff that happens at load time in nfasload.
defines find-package, set-package, pkg-arg, register-package-ref as possibly 

Ok, wait, l1-symhash uses stuff:
  %htab-add-symbol
  %find-symbol
Maybe others.  But nobody else uses pkg.itab/pkg.etab!

(defvar *package-refs*)
(setq *package-refs* (make-hash-table :test #'equal))
(defvar *package-refs-lock*)
(setq *package-refs-lock* (make-lock))

(dolist (p %all-packages%)
  (dolist (name (pkg.names p))
    (setf (package-ref.pkg (register-package-ref name)) p)))

(let* ((force-export-packages (list *keyword-package*))
       (force-export-packages-lock (make-lock)))
  (defun force-export-packages ()
    (with-lock-grabbed (force-export-packages-lock)
      (copy-list force-export-packages)))
  (defun package-force-export (p)
    (let* ((pkg (pkg-arg p)))
      (with-lock-grabbed (force-export-packages-lock)
        (pushnew pkg force-export-packages))
    pkg))
  (defun force-export-package-p (pkg)
    (with-lock-grabbed (force-export-packages-lock)
      (if (memq pkg force-export-packages)
        t))))
|#

(defun cvmload-level-0 (files)
  (let ((calls
         (loop for file in files
           unless (string-equal (pathname-name file) "nfasload")
           collect (let ((*deferred-level-0-calls* (list file)))
                     (cvmload file)
                     (nreverse *deferred-level-0-calls*)))))
    (FORMAT T "~&LOADED ~s files, HAVE ~s calls" 
            (length calls)
            (loop for info in calls sum (length (cdr info))))
   ;; Some stuff xfasload inits
    (%defvar (ccl '*ccl-package*) () 'variable *ccl-pkg*)
    (%defvar (ccl '*common-lisp-package*) () 'variable *cl-pkg*)
    (%defconstant (ccl '%unbound-function%) *unbound-function*)
    (%defvar (ccl '*package*) () 'variable *ccl-pkg*)
    (%defvar (ccl '*keyword-package*) () 'variable *keyword-pkg*)
    (%defvar (ccl'*gc-event-status-bits*) () 'variable 0)
    (%defvar (ccl '%toplevel-catch%) () 'variable (ccl :toplevel))
    ; %closure-code%, %macro-code%, %builtin-functions%
    ;;(setf (xload-symbol-value (xload-copy-symbol '*xload-cold-load-documentation*))
    ;;      (xload-save-list (setq *xload-cold-load-documentation*
    ;;                             (nreverse *xload-cold-load-documentation*))))
    (loop for info in calls
      do (format t "~2&~s CALLS FOR FILE ~s" (length (cdr info)) (car info))
      do (loop for fn in (cdr info) for index upfrom 1
           do (format t "~&  Call #~s" index)
           do (ccl-funcall fn)))

    ;;;; TODO******* So this needs to somehow come in from the compiler, because that's who knowns where it puts it.
    (%defvar (ccl '*xload-startup-file*) () 'variable (ccl "level-1.cvmsrc"))
    (%defvar (ccl '*openmcl-svn-revision*) () 'variable nil) ;; (local-vc-revision) -- SO THIS NEEDS TO BE FROM COMPILE/XLOAD time again
    (%defvar (ccl '*optional-features*) () 'variable nil) ;(mapcar 'ccl-symbol CCL::*BUILD-TIME-OPTIONAL-FEATURES*)

    (unbootstrap-documentation)
    ;;(unbootstrap-packages)
    ;;(%fasload *xload-startup-file*))
    ;;; FIrst few files.
    ;(l1-load "l1-cl-package")
    ;(l1-load "l1-utils")
    ;(l1-load "l1-init")
    ;(l1-load "l1-symhash")
    ;(l1-load "l1-numbers")
    ;(l1-load "l1-aprims")
    ))


    ;; ** ONCE SWITCH TO native packages, lookup won't be so fast.  Maybe have a cache of
    ;; all the stuff that goes through ccl-symbol, don't need to support shadowing and
    ;; such just to run the compiler.


  

(defun cvmload  (file)
  (assert (equal (pathname-type file) "cvmsrc"))
  ;; Should we compile then load?  Only worth if can avoid the compile!
  ;; Which means we need to figure out fasl file conventions in the lisp.
  ;; Worry about it later
  (let ((*loader-table* nil)
        (*package* (find-package :ccl-vm)))
    ;;; TODO: need to ccl-bind *package* so can then set it.
    (declare (special *loader-table*))
    (load file)))

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


(defun $fasl-set-package (str)
  (fasl-trace "~s ~s" '$fasl-set-package str)
  (check-type str ccl-simple-base-string)
  (let ((pkg (pkg-arg str)))
    (assert (eq pkg *ccl-pkg*))
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

#|
;;This is all we need for loading level-0, aside from toplevel fns, to get all the arguments.
$bs-package $bs-symbol
$bs-cons-function
$bs-init-function $bs-istruct-cell
$bs-quote $bs-make-uvector
 $bs-init-uvector
$bs-gvector $bs-uvector $bs-eval)
|#

(defun $fs-unbound-marker () *unbound-marker*)
(defun $fs-slot-unbound-marker () *slot-unbound-marker*)
(defun $fs-illegal-marker () *illegal-marker*)

(defun $fs-package (name)
  (fasl-trace "   ~s ~s" '$fs-package name)
  (check-type name ccl-simple-base-string)
  (pkg-arg name))

(defun $fs-symbol (name pkg)
  (fasl-trace "   ~s ~s ~s" '$fs-symbol name pkg)
  (find-or-make-sym name pkg))

(defun $fs-string (string)
  (check-type string string)
  (ccl-string string))

(defun $fs-make-uvector (type-key size)
  (fasl-trace "   ~s ~s ~s" '$fs-make-uvector type-key size)
  (check-type size fixnum)
  (make-uvector size (typekey-subtag type-key)))

(defun $fs-init-bslambda (bslambda)
  ;; We $BS-QUOTED the name and the keywords so as do get the fasdumper to do the right thing,
  ;; but don't want to have to always bseval them.
  (flet ((unquot (thing)
           (if (and (consp thing) (consp (cdr thing)) (null (cddr thing))
                    (eq (car thing) '$bs-quote)
                    ;(typep (cadr thing) 'ccl-symbol)
                    )
             (cadr thing)
             (error "Expected a quoted object not ~s" thing))))
    (destructuring-bind (name (inh req opt rest keys) body num) (cdr bslambda)
      (declare (ignore inh req opt rest body num))
      (setf (cadr bslambda) (unquot name))
      (loop for info in (cdr keys)
        do (destructuring-bind (key var init supp) info
             (declare (ignore var init supp))
             (setf (car info) (unquot key)))))
    bslambda))
                   

(defun $fs-cons-function ()
  (fasl-trace "   ~s" '$fs-cons-function)
  (cons-ccl-function))

(defun $fs-init-function (fn bslambda bits)
  (let ((*print-length* 3) (*print-level* 3))
  (fasl-trace "   ~s ~s ~s ~s" '$fs-init-function fn bslambda bits))
  (init-ccl-function fn bslambda bits))

(defun $fs-init-uvector (uvec &rest values)
  (fasl-trace "   ~s ~s ~s" '$fs-init-uvector uvec values)
  (let ((vec (uvector uvec)))
    ;; so the values should be like going through $BS-QUOTE, because they could be numbers, e.g.
    ;; bignums.
    (assert (eq (length vec) (length values)))
    (loop for val in values as index upfrom 0
      do (setf (aref vec index) (ccl val)))
    uvec))

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
  (break "NIY")
  'eval-not-implemented-yet)

;; like $fasl-funcall but for value, it's used in load-time values.
(defun $fs-funcall (fn-expr)
  (fasl-trace "   ~s ~s" '$fs-funcall fn-expr)
  (break "NIY")
  'funcall-not-implemented-yet)

(defvar *istruct-cells-sym*  (ccl '*istruct-cells*))
(%defvar *istruct-cells-sym* () 'variable nil)

(defun $fs-istruct-cell (sym)
  (fasl-trace "   ~s ~s" '$fs-istruct-cell sym)
  (check-type sym ccl-symbol)
  ;; Could switch to use ccl register-istruct-cells once it's defined, but why bother..
  ;; (if (fboundp (ccl'register-istruct-cell)) (ccl-funcall (ccl'register-istruct-cell) sym) ...)
  (let ((alist (sym-value *istruct-cells-sym*)))
    (or (assoc sym alist)
        (let ((pair (cons sym nil)))
          (setf (sym-value *istruct-cells-sym*) (cons pair alist))
          pair))))
