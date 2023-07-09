(in-package :ccl-vm)

;;;; *** TODO: there are too many forward references in level-0. 
(defvar *deferred-level-0-calls* nil)

;;;; *** TODO: need to kludge around this - on one hand we want fasl-pathanme to be in
;;; the backend, so produce the right files, but don't want to load them as fasls!!
;;; instead of using backend-target-fals-pathname in bscompile, just kludge something.
(defun load-as-source (file)
  ;; If we happen to have cvm-backend loaded in the same lisp, ccl won't load our files from source.
  (if #+ccl (boundp 'ccl::*cvm-backend*)  #-ccl nil
    (let ((fasl (ccl::backend-target-fasl-pathname ccl::*cvm-backend*)))
      (unwind-protect
          (progn
            (setf (ccl::backend-target-fasl-pathname ccl::*cvm-backend*) #P".NOTHING-TO-SEE-HERE")
            (load file))
        (setf (ccl::backend-target-fasl-pathname ccl::*cvm-backend*) fasl)))
    (load file)))

(defun cvmload-level-0 (files)
  (let ((calls
         (loop for file in files
           collect (let ((*deferred-level-0-calls* (list file)))
                     (cvmload file)
                     (nreverse *deferred-level-0-calls*)))))
    (FORMAT T "~&LOADED ~s files, HAVE ~s calls" 
            (length calls)
            (loop for info in calls sum (length (cdr info))))
    (loop for info in calls
      do (format t "~2&~s CALLS FOR FILE ~s" (length (cdr info)) (car info))
      do (loop for fn in (cdr info) for index upfrom 1
           do (format t "~&  Call #~s" index)
           do (ccl-funcall fn)))))


(defun cvmload  (file)
  (assert (equal (pathname-type file) "cvmfsl"))
  ;; Should we compile then load?  Only worth if can avoid the compile!
  ;; Which means we need to figure out fasl file conventions in the lisp.
  ;; Worry about it later
  (let ((*loader-table* nil)
        (*package* (find-package :ccl-vm)))
    ;;; TODO: need to ccl-bind *package* so can then set it.
    (declare (special *loader-table*))
    (load-as-source file)))

;; a CVMFSL file is a bunch of toplevel calls to these $fasl functions.  The arguments
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
  ;; (cadr doc) is position '&body in arglist, for defindent...
  (let ((arglist (and (listp doc) (prog1 (cddr doc) (setq doc (car doc))))))
    (check-type fn ccl-function)
    (let ((sym (ccl-function-name fn)))
      (check-type sym ccl-symbol)
      (record-debug-info sym doc 'function arglist)
      (ccl-set-macro-function sym fn))))

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

(defun $fs-symbol (name pkg binding-p)
  (fasl-trace "   ~s ~s ~s ~s" '$fs-symbol name pkg binding-p)
  (let* ((sym (find-or-make-sym name pkg)))
    (when binding-p
      (ensure-binding-index sym))
    sym))

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

(defun $fs-istruct-cell (sym)
  (fasl-trace "   ~s ~s" '$fs-istruct-cell sym)
  (check-type sym ccl-symbol)
  (register-istruct-cell sym))

;;; This is actually set to an alist in the xloader.
(defparameter *istruct-cells* nil) ;; will need to move it to the ccl variable
;;; This should only ever push anything on the list in the cold
;;; load (e.g., when running single-threaded.)
;; huh? ^?  it's called all over!!
(defun register-istruct-cell (sym) ;; this is defined in l0-pred, doesn't seem to be redefined anywhere
  (or (assoc sym *istruct-cells*)
      (let ((pair (cons sym nil)))
        (push pair *istruct-cells*)
        pair)))

;; the cdr gets filled in when classes are created, in l1-clos-boot.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  STARTUP 
;;;;


;;;; Ok, as load up, first l0-aprims - first thing is, makes CCL::SET-PACKAGE symbol and funcalls it!
;;;  then some defuns.  then call 

#|
;; See ccl::xload-initial-packages
;; target is like "X8664" and os is like "X86-Darwin 4"
(defvar *initial-packages* '("CL" "CCL"  "KEYWORD" "TARGET" "OS"))

;; So it takes all the package info as in fully loaded lisp.  copy-list of pkg.names
;; and used and used-by from CCL!  not fixed.  It can be a source file, or a directly
;; generated bseval file.

;; ok, so in CCL will have to generate a bootstrap file, that will have things
;; like builting the packages, from CCL.

;; OK, in the VM, make a "BOOTSTRAP_VM" package, and have it some things in it,
;; like init-packages
(defun bscompile-init-packages-form ()
  (let* ((packages (mapcar #'find-package *initial-packages*))
         (descs (loop for p in init-packages
                  do (assert (null (pkg.shadowed p)))
                  collect (list (package-name p)
                                (length (htvec (pkg.itab p)))
                                (htlimit (pkg.itab p))
                                (length (htvec (pkg.etab p)))
                                (htlimit (pkg.etab p))
                                (pkg.names p)
                                (loop for used in (pkg.used p)
                                  do (assert (find used packages))
                                  collect (package-name used))
                                (loop for user in (pkg.used-by p)
                                  when (find user packages) collect user)))))
    `(progn
       (vm-bootstrap::init-packages ',descs)
       (setq cl::*package* 
      (ccl-set (ccl-symbol "CL:*PACKAGE*") (find-pkg "CCL"))
      (ccl-set (ccl-symbol "CCL:*KEYWORD-PACKAGE*") (find-pkg "KEYWORD"))
      (ccl-set (ccl-symbol "CCL:%ALL-PACKAGES%") (mapcar #'cadr alist))
      (ccl-set (ccl-symbol "CCL:%UNBOUND-FUNCTION") <>)
               ;; It depends on being the frist thing allocated!!!
                 (+ *xload-dynamic-space-address* *xload-target-fulltag-misc*)
      (ccl-set (ccl-symbol "CCL:%TOPLEVEL-CATCH%") (ccl-symbol "KEYWORD:TOPLEVEL"))
      (ccl-set (ccl-symbol "CCL:%CLOSURE-CODE%") <trampoline code>)
      (ccl-set (ccl-symbol "CCL:%MACRO-CODE%") (backend-xload-info-macro-apply-code-function
                                                ))
      ;; for kernel callbacks, we don't need this
      (ccl-set (ccl-symbol "CCL:%BUILTIN-FUNCTIONS%") ..)
      ;; set startup file.
      <xfasload ocmpiled levle 0 files>
      (xload-set "CCL:*XLOAD-STARTUP-FILE*" <the string>)
      check that %TOPLEVEL-FUNCTION% got set
      store l*xload-cold-load-functions*
      store *early-class-cells*
      features
      set xload-load-doc
      (dolist (s *xload-reserved-special-binding-index-symbols*)




(defun vm-bootstrap::init (package-inits)
  (init-packages package-inits)
  ;;  not really clear how this gets remembered?
  (create-undefined-function-object)
  ;(ccl-gvector :simple-vector <function-that-reports-unbound-function error>)

  (defun xload-nrs ()

;; Symbols that are accessible from the kernel.
(defparameter *x86-nil-relative-symbols*
  '(t
    nil
    ccl::%err-disp
    ccl::cmain
    eval
    ccl::apply-evaluated-function
    error    
    ccl::%defun
    ccl::%defvar
    ccl::%defconstant
    ccl::%macro
    ccl::%kernel-restart
    *package*
    ccl::*total-bytes-freed*
    :allow-other-keys    
    ccl::%toplevel-catch%
    ccl::%toplevel-function%
    ccl::%pascal-functions%    
    ccl::restore-lisp-pointers
    ccl::*total-gc-microseconds*
    ccl::%builtin-functions%
    ccl::%unbound-function%
    ccl::%init-misc
    ccl::%macro-code%
    ccl::%closure-code%
    ccl::%new-gcable-ptr
    ccl::*gc-event-status-bits*
    ccl::*post-gc-hook*
    ccl::%handlers%
    ccl::%all-packages% ;;; <<<<<
    ccl::*keyword-package* ;;; <<<<<<
    ccl::%os-init-function%
    ccl::%foreign-thread-control
    ))

  (mapcar
   #'(lambda (s)
       (or (assq s '((nil)
                     (%pascal-functions%)
                     (*all-metered-functions*)
                     (*post-gc-hook*)
                     (%handlers%) 
		     (%finalization-alist%)
                     (%closure-code%)))
	   s))
   (backend-xload-info-nil-relative-symbols *target-backend*)))
  ;; load these symbols into static space, so they have fixed addresses
  ;; preserve constantness,  for the few above, set them to nil

;; kludge - the undefined function object is the first thing in static space,
;; that's how it's found!!
 (xload-save-code-vector
                 (backend-xload-info-udf-code
                  *xload-target-backend*))

  (make-unbound-function-object)
  (ccl-set (ccl-symbol '*package*) 


#|
This needs cleanup on native vs ccl stuff, but shows it won't be hard to have a
builtin intern until l1-symhash is loaded.  It will be the same as the ccl intern
because we have the same HASH-PNAME.  We do still have to copy mixup-hash-code.

Now here's a thing.  When cross compiling, mixup-hash-code is null.

Ok, so we are running in a normal lisp, and have loaded up stuff, including a normal
mixup-hash-code function.  Now we rebuild level with a different target, so
compile-file will force cross-compiling on features,  So we compile l0-hash with a noop
MIXUP-HASH-CODE.  Then we're xloading this level-0, and in the xfasloader, we make
the clone packages and put stuff in them using the full mixup-hash-code, and then we
copy the tables into the image!  how does this ever work?


Ok, a safer way is to just have a bootstrapping intern

As we're fasloading, until we get any explicit calls to INTERN, we're using $FASL-INTERN (or whatever)
for any symbols that are loaded.  

INTERN and %PKG-REF-INTERN (comes out of compiler optimizer) are in l1-symhash!
   they call %find-symbol and %add-symbol which are in nfasload.
So as we're loading level-0, we can only do the fasloader intern.  at end of level-0,
call UPGRADE PACKAGES, and then as soon as the ccl intern is defined, can start calling it.

;; This needs to be dealing native syms, native paackges, 
(defun native-intern (str ccl-package)
  (flet ((hash-pname (str)
           (let ((len (length str)))
             (MIXUP-HASH-CODE (%PNAME-HASH str len))))
         (lookup (str htab primary secondary)
           (let* ((vec (htvec htab))
                  (vlen (length vec))
                  (secondary (aref $hprimes (logand primary 7))))
             (loop
               for next = primary then (+ idx secondary) as idx = (mod next vlen)
               do (let ((elt (svref vec idx)))
                    (when (eql elt 0) (return idx))
                    (when (string= (symbol-name elt) str) (return elt))))))
         (add-sym (str package htab idx)
           (let ((sym (make-symbol str)))
             (setf (%svref symvec target::symbol.package-predicate-cell) package)
             (setf (svref (htvec htab) idx) sym)
             (when (>= (incf (htcount htab)) (htlimit htab))
               (error "Can't grow htab while bootstrapping"))
             sym)))
    (let ((pvec (ccl-uvector-array ccl-package))
          (hash (HASH-PNAME str (length str))))
      (if (keyword-package-p ccl-package)
        (let ((sym-or-etab-idx (lookup str (pkg.etab package) hash)))
          (if (symbolp sym-or-etab-idx)
            (values sym-or-etab-idx :external)
            (let ((sym (add-sym str package (Pkg.etab package) sym-or-etab-idx)))
              (%set-sym-global-value sym sym)
              (%symbol-bits symbol 
                            (logior (ash 1 $sym_vbit_special) 
                                    (ash 1 $sym_vbit_const)
                                    (the fixnum (%symbol-bits symbol)))))))
        (let ((sym-or-itab-idx (lookup str (pkg.itab package) hash)))
          (if (symbolp sym-or-itab-idx)
            (values sym-or-itab-idx :internal)
            (let ((sym-or-itab-idx (lookup str (pkg.etab package) hash)))
              (if (symbolp sym-or-itab-idx)
                (values sym-or-etab-idx :external)
                (loop for p in (pkg.used package)
                  do (let ((sym-or-idx (lookup str (pkg.etab p) hash)))
                       (when found-p
                         (return (values sym-or-idx :inherited))))
                  finally (return
                           (values (add-sym str package (pkg.itab package) sym-or-itab-idx)
                                   :internal)))))))))))
|#
    ;; 

;; Option (1) %pname-hash, which is written in lap anyway in x86-symbol.lisp
;; will be built in the kernel, CVM-symbol won't have it, so we can call it before
;; CVM-symbol is loaded.
;; -- Ok, have a native INTERN, that gets replaced by the ccl intern when L1-symhash gets
;; loaded.But it will store things the same way because they share the same %pname-hash.
;;  Can skip all the table growing stuff because won't happen before L1-symhash.
;;; ALSO:
;;;  have native "MIRROR-CCL" package, whenever intern anything in CCL, intern the equivalent
;;; symbol in the native package, and also have it track the function, so the native
;;; symbol is fbound to the native fn of the ccl fn that the ccl sym is fbound to.
;;; So can just call ccl-mirror:mumble to call ccl mumble symbol.  If don't need to
;;; track the value, could have the value be the CCL-SYMBOL.  Or could put it on the PLIST.



    (do* ((idx (fast-mod primary vlen) (+ i secondary))
          (i idx (if (>= idx vlen) (- idx vlen) idx))
          (elt (svref vec i) (svref vec i)))
         ((eql elt 0) (values nil nil i))
      (declare (fixnum i idx))
      (when (symbolp elt)
        (let* ((pname (symbol-name elt)))
          (if (and 
               (= (the fixnum (length pname)) len)
               (dotimes (j len t)
                 (unless (eq (schar str j) (schar pname j))
                   (return))))
            (return (values t (%symptr->symbol elt) i))))))))
(defun %intern (str package)
 (%add-symbol str package internal-offset external-offset)


   (multiple-value-bind (symbol where internal-offset external-offset) 
                        (multiple-value-bind (found-p sym internal-offset)
                                             (%get-htab-symbol string len (pkg.itab package))
                          (if found-p
                            (values sym :internal internal-offset nil)
                            (multiple-value-bind (found-p sym external-offset)
                                                 (%get-htab-symbol string len (pkg.etab package))
                              (if found-p
                                (values sym :external internal-offset external-offset)
                                (dolist (p (pkg.used package) (values nil nil internal-offset external-offset))
                                  (multiple-value-bind (found-p sym)
                                                       (%get-htab-symbol string len (pkg.etab p))
                                    (when found-p
                                      (return (values sym :inherited internal-offset external-offset)))))))))
     (if where
       (values symbol where)
       (values (%add-symbol str package internal-offset external-offset) nil)))))

;; These are decided from the CCL source, so the data must be output somewhere
(defun vm-boostrap::init-packages (descs)
  (let ((alist (loop for desc in descs
                 (destructuring-bind (name itab-size itab-limit
                                           etab-size etab-limit
                                           names used-names used-by-names) desc
                   (declare (ignore used-names used-by-names))
                   (list name
                         (ccl-gvector (ccl-symbol :package)
                                      (list* (ccl-make-array itab-size :initial-element 0) 0 itab-limit)
                                      (list* (ccl-make-array etab-size :initial-element 0) 0 etab-limit)
                                      nil
                                      nil
                                      (copy-list names)
                                      nil
                                      nil
                                      nil)
                         used-names used-by-names)))))
    (flet ((find-pkg (name)
             (cadr (require-type (assoc name alist :test 'equal) 'cons)))
           (get-htab-symbol (htab name)
             (defun %get-htab-symbol (string len htab)

               


  (multiple-value-bind (p s) (hash-pname string len)
    (%get-hashed-htab-symbol string len htab p s)))


      (loop for (name pkg used-names used-by-names) in alist
        do (setf (pkg.used pkg) (mapcar #'find-pkg used-names))
        do (setf (pkg.used-by pkg) (mapcar #'find-pkg used-by-names)))
      (loop for (pkg-name sym-name) in *early-symbols*
        as pkg = (find-pkg pkg-name)
        as 

    (setq *boostrap-packages* (mapcar #'cadr alist))




))


      ;; This can be done nmormallly

      ;; (bootstrap-symbol pkg name value
      (ccl-set (ccl-symbol "CL:*PACKAGE*") (find-pkg "CCL"))
      (ccl-set (ccl-symbol "CCL:*KEYWORD-PACKAGE*") (find-pkg "KEYWORD"))
      (ccl-set (ccl-symbol "CCL:%ALL-PACKAGES%") (mapcar #'cadr alist))
      (ccl-set (ccl-symbol "CCL:%UNBOUND-FUNCTION") <>)
               ;; It depends on being the frist thing allocated!!!
                 (+ *xload-dynamic-space-address* *xload-target-fulltag-misc*)
      (ccl-set (ccl-symbol "CCL:%TOPLEVEL-CATCH%") (ccl-symbol "KEYWORD:TOPLEVEL"))
      (ccl-set (ccl-symbol "CCL:%CLOSURE-CODE%") <trampoline code>)
      (ccl-set (ccl-symbol "CCL:%MACRO-CODE%") (backend-xload-info-macro-apply-code-function
                                                ))
      ;; for kernel callbacks, we don't need this
      (ccl-set (ccl-symbol "CCL:%BUILTIN-FUNCTIONS%") ..)
      ;; set startup file.
      <xfasload ocmpiled levle 0 files>
      (xload-set "CCL:*XLOAD-STARTUP-FILE*" <the string>)
      check that %TOPLEVEL-FUNCTION% got set
      store l*xload-cold-load-functions*
      store *early-class-cells*
      features
      set xload-load-doc
      (dolist (s *xload-reserved-special-binding-index-symbols*)
        (xload-ensure-binding-index (store-symbol s)))
      
      
               


(init-package '#.(find-package "CL")
              :itab-limit <>
              :etab-limit <>
              :names '(....)
              ;; These could be direct references to the package
              :used-by '(...) ;; only in the predefined
              :used '(...))
 this gets compiled into (a) $BS-PACKAGE name for first refs to each package
(b) ($funcall (symbol "Init-package" "CCL") ..)
so have to kludge around INIT_PACKAGE since can't be interned yet.
I mean it can, but then not clean init.
One thing might be for first ref to any package, can pass in all the info...

Start out with a CCL package with some predefined symbols and let it grow naturally,
i.e. don't worry about the etab-limit.


(defun init-boostrap-packages ()
  (loop for p in *initial-packages*
    collect `($BS-BOOT-PACKAGE (htlimit (pkg.itab p)) ;;macro from nfasload
                               (htlimit (pkg.etab p))
                               (pkg.names p)))
  (loop for p in *initial-packages*
    collect `($BS-INIT-PACKAGE p
                               (mapcar #'package-name (pkg.used p))
                               (mapcar #'package-name (pkg.used-by p)))))


(*xload-package-alist* (xload-clone-packages (xload-initial-packages)))
(defun xload-initial-packages ()
  (mapcar #'find-package ))
|#

#|
Ok, so:

In CCL, we have BSCOMPILE running as an alternate backend.  It will be a new target, darwin-vm: vm is the architecture,
and darwin is the OS.  We have to write a "file compiler" that will output source files.

will be running, say SBCL, Lispworks, or, for testing, CCL.

In the VM, we initialize the VM, which is native CL code with backend for specialized stuff. The VM includes
its own FASLOADER, which will load the bs-fasls and make the objects.

|#

