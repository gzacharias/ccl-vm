(in-package :ccl-vm)

;; Don't load nfasload!  We don't plan to use it, we just want to be able to
;;  use the compiler and we'll be using our loader.

;; Then can replace the stuff in there that's used elsewhere with LAP, and
;;  keep packages fast.
#|  Stuff that happens at load time in nfasload.
defines find-package, set-package, pkg-arg, register-package-ref as possibly 

Ok, wait, l1-symhash uses stuff:
  %htab-add-symbol
  %find-symbol
Maybe others.  who else uses pkg.itab/pkg.etab!

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
;;;;; For testing only
(import 'ccl::test-load :ccl-vm)
(import 'ccl::test-vm :ccl-vm)
(defun ccl::test-load ()
  ;; Don't really understand the intended way of doing this.  Any attempt to
  ;; use a target ends up calling FIND-BACKEND, but there is no cvm backend until
  ;; these files are loaded, so just do it.
  ;(load "ccl:compiler;cvm;cvm-arch.lisp")
  ;(load "ccl:compiler;cvm;cvm-backend.lisp")
  (cl-user::load-cvm)
  (cvmload-ccl))

(defvar *CCL-DIRECTORY*)

(defun cvmload-ccl ()
  ;; Load level-0
  (let* ((files (sort (directory "ccl:cvmsrcs;level-0;*.cvmsrc") #'string-lessp :key #'pathname-name))
         (calls (loop for file in files
                  unless (string-equal (pathname-name file) "nfasload")
                  nconc (let ((*deferred-level-0-calls* (list t)))
                          (cvmload file)
                          (loop for call in (cdr (nreverse *deferred-level-0-calls*))
                            collect (list file call))))))
    ;; However the real load happens, have to record the ccl directory so the vm can find it.
    (SETQ *CCL-DIRECTORY* (truename "ccl:"))
    
    ;; Some stuff xfasload inits
    ;; Most of this could be done before loading level-0!
    (%defvar (ccl-symbol '*package*) () 'variable *ccl-pkg*)
    (%defvar (ccl '*ccl-package*) () 'variable *ccl-pkg*)
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
    ;; default to unshared hash tables, lock-free-puthash seems to get an infinite loop **** TRACK THIS DOWN
    ;;  Have to do this before %documentation is initialized, in level-0!
    (setf (sym-value (ccl '*shared-hash-table-default*)) nil)
    (setf (sym-value (ccl '*current-process*)) 1234) ;; needed for non-shared hash tables.
    
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
    ;;  Here's what level-1.lisp would load
    (format t "~&Level-0 loaded~%")
    
    ;; Here might also want to replace some l0-hash table fns with speedier versions?
    
    
    ;; (l1-load "l1-cl-package") - just does CL package, which we pre-allocated.
    (pretend-fasload "l1-utils")
    (pretend-fasload "l1-init")
    (pretend-fasload "l1-symhash")
    (pretend-fasload "l1-numbers")
    (pretend-fasload "l1-aprims")
    ;; (l1-load "x86-callback-support")
    (pretend-fasload "l1-callbacks")
    (pretend-fasload "l1-sort")
    (pretend-fasload "lists")
    (pretend-fasload "sequences")
    (pretend-fasload "l1-dcode")
    (pretend-fasload "l1-clos-boot")
    (pretend-fasload "hash")
    (pretend-fasload "l1-clos")
    (pretend-fasload "defstruct")
    (pretend-fasload "dll-node")
    (pretend-fasload "l1-unicode")
    (pretend-fasload "l1-streams")
    ;; Ok this does defstruct which calls definition-environment which is defined in l1-readloop.
    ;; how does this ever work?  Ok, it only seems to call it on shared-resource-request,
    ;; which is the first one that does an :include
    (pretend-fasload "linux-files")
    (pretend-fasload "chars")
    (pretend-fasload "l1-files")
    (let ((provide (sym-func (ccl 'provide))))
      (ccl-funcall provide (ccl-string "SEQUENCES"))
      (ccl-funcall provide (ccl-string "DEFSTRUCT"))
      (ccl-funcall provide (ccl-string "CHARS"))
      (ccl-funcall provide (ccl-string "LISTS"))
      (ccl-funcall provide (ccl-string "DLL-NODE")))
    (pretend-fasload "l1-typesys")
    (pretend-fasload "sysutils")
    ;; (l1-load "x86-threads-utils")
    ;; Really should skip processes if skip threads...
    (pretend-fasload "l1-lisp-threads")
    (pretend-fasload "l1-application")
    (pretend-fasload "l1-processes")
    (pretend-fasload "l1-io")
    (pretend-fasload "l1-reader")
    (pretend-fasload "l1-readloop")
    (pretend-fasload "l1-readloop-lds")
    (pretend-fasload "l1-error-system")
    (pretend-fasload "l1-events")
    ;; (l1-load "x86-trap-support")
    (pretend-fasload "l1-format")
    (pretend-fasload "l1-sysio")
    (pretend-fasload "l1-pathnames")
    (pretend-fasload "l1-boot-lds")
    (pretend-fasload "l1-boot-1")
    (pretend-fasload "l1-boot-2")
    (pretend-fasload "l1-boot-3")
    
    
    ))

;; called from lap-%fasload.
(defun pretend-fasload (filename)
  (let ((file (make-pathname :name (pathname-name filename) :defaults "ccl:cvmsrcs;.cvmsrc")))
    (if (probe-file file)
      (progn (cvmload file) t)
      (progn (format t "~2&SKIPPING ~s~2%" filename) nil))))


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

(defun $fs-unbound-marker () *unbound-marker*)
(defun $fs-slot-unbound-marker () *slot-unbound-marker*)
(defun $fs-illegal-marker () *illegal-marker*)

(defun $fs-package (name)
  (fasl-trace "   ~s ~s" '$fs-package name)
  (check-type name ccl-simple-base-string)
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
  (let ((vec (uvector-data uvec)))
    ;; so the values should be like going through $BS-QUOTE, because they could be e.g. bignums.
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
;;; I BELEIVE *ALL* calls to this a find-class-cell, maybe its worth breaking out,
;;; even just to call out to ccl.
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
