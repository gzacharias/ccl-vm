(in-package :ccl-vm)

(deftype ccl-fixnum () `(signed-byte ,num-fixnum-bits))

(defparameter *subtag-consers* ())

;; a simple vector for everything, until there's a good reason not to.
(defstruct ccl-uvector
  (subtag 0 :type (unsigned-byte 8) :read-only t)
  (data #() :type simple-vector))

(defmacro def-uvector-subtype (typekey struct &rest slots)
  (let ((name (if (consp struct) (car struct) struct))
        (options (if (consp struct) (cdr struct) nil))
        (include 'ccl-uvector)
        (constructor nil)
        (subtag-conser nil))
    (loop for option in options
      do (destructuring-bind (key val) option
           (ecase key
             (:include (setq include val))
             (:constructor (setq constructor val))
             (:subtag-conser (setq subtag-conser val)))))
    (when (null constructor)
      (setq constructor (intern (concatenate 'string "MAKE-" (string name)))))
    (when (eq subtag-conser t) (setq subtag-conser constructor))
    (when (null subtag-conser) (setq subtag-conser 'error))
    `(progn
       (defstruct (,name (:include ,include) (:constructor ,constructor))
         ,@slots)
       (push '(,(typekey-subtag typekey) . ,subtag-conser) *subtag-consers*))))


(defmethod print-object ((obj ccl-uvector) stream)
  (let ((type (subtag-typekey (ccl-uvector-subtag obj))))
    (format stream "<~s ~s " (type-of obj) type)
    (print-uvector-data type obj stream)
    (format stream ">")))

(defmethod print-uvector-data ((type t) obj stream)
  (format stream "~s elts" (length (ccl-uvector-data obj))))

(declaim (inline uvector))
(defun uvector (obj)
  (ccl-uvector-data (if (typep obj 'boolean) (sym-symvector obj) obj)))

(defun uvector-equal (uv1 uv2) ;; true if same type and all data values are eql.
  (and (eql (ccl-uvector-subtag uv1)
            (ccl-uvector-subtag uv2))
       (let ((v1 (ccl-uvector-data uv1))
             (v2 (ccl-uvector-data uv2)))
         (and (eql (length v1) (length v2))
              (every #'eql v1 v2)))))

(defun gvref (uvec index)
  (unless (and (ccl-uvector-p uvec)
           (gvector-type-p (ccl-uvector-subtag uvec)))
    (report-bad-arg uvec 'gvector))
  (uvref uvec index))

(defun gvset (uvec index val)
  (unless (and (ccl-uvector-p uvec)
               (gvector-type-p (ccl-uvector-subtag uvec)))
    (report-bad-arg uvec 'gvector))
  (uvset uvec index val))

(defun uvref (uvec index)
  (check-type uvec ccl-uvector)
  (svref (ccl-uvector-data uvec) index))

(defun uvset (uvec index val)
  (check-type uvec ccl-uvector)
  (check-type val ccl-object)
  (setf (svref (ccl-uvector-data uvec) index) val))

(defun uvsize (uvec)
  (check-type uvec ccl-uvector)
  (length (ccl-uvector-data uvec)))

;; we need certain vectors to have a host class of their own so can use typecase,
;;  give them print methods, etc.

(def-uvector-subtype :symbol (ccl-symvector (:constructor %make-ccl-symvector) (:subtag-conser t)))
  
(deftype ccl-symbol () '(or boolean ccl-symvector))

(def-uvector-subtype :function (ccl-function (:constructor %make-ccl-function) (:subtag-conser nil))
  (bslambda () :type (or list (eql lap)))
  ;; The native function takes 3 arguments:
  ;; (1) outer env (which is not really used but is there to provide a stack for debugging)
  ;; (2) ccl-function object, for self call and debugging
  ;; (3) the list of arguments
  (native-fn () :type (or null compiled-function)))


(def-uvector-subtype :simple-string (ccl-simple-base-string (:subtag-conser t)))

(defmethod print-uvector-data ((type (eql :simple-string)) obj stream)
  (prin1 (coerce (ccl-uvector-data obj) 'string) stream))

(def-uvector-subtype :bignum (ccl-bignum (:subtag-conser t)))

(deftype ccl-integer () `(or ccl-fixnum ccl-bignum))

(def-uvector-subtype :macptr (ccl-macptr (:constructor %make-ccl-macptr) (:subtag-conser t)))

(defconstant macptr.address-cell 0) ;; this contains raw native (unsigned-byte 64).
;(defconstant macptr.domain-cell 1)
;(defconstant macptr.type-cell 2)

(defun make-ccl-macptr (native-value &optional gc-flags)
  (check-type native-value (or (signed-byte 65) cffi:foreign-pointer))
  (let* ((address (if (integerp native-value)
                    (logand native-value #xFFFFFFFFFFFFFFFF)
                    (cffi:pointer-address native-value)))
         (ptr (%make-ccl-macptr :subtag subtag-macptr
                                :data (if gc-flags
                                        (vector address 0 0 gc-flags 0)
                                        (vector address 0 0)))))
    (when gc-flags
      (lap-set-%gcable-macptrs% ptr))
    ptr))

(defun %macptr-value (ptr) ;; returns raw native (unsigned-byte 64)
  (check-type ptr ccl-macptr)
  (svref (uvector ptr) macptr.address-cell))
  
(defun %macptr-ptr (macptr)  ;; value as a native pointer
  (cffi:make-pointer (%macptr-value macptr)))

(defun (setf %macptr-value) (value ptr)
  (check-type ptr ccl-macptr)
  (check-type value (or integer cffi:foreign-pointer))
  (setf (svref (uvector ptr) macptr.address-cell)
        (if (integerp value)
          (logand #xFFFFFFFFFFFFFFFF value)
          (cffi:pointer-address value))))


(defun fulltag (obj) ;; returns 4 bit typecode.
  (etypecase obj
    (ccl-fixnum (if (evenp obj) fulltag-even-fixnum fulltag-odd-fixnum))
    (null fulltag-nil)
    (list fulltag-cons)
    (character fulltag-character)
    (single-float fulltag-single-float)
    ((eql t) fulltag-symbol)
    (ccl-symvector fulltag-symbol)
    (ccl-function fulltag-function)
    (ccl-uvector fulltag-misc)
    (t (cond ((or (eq obj *unbound-marker*)
                  (eq obj *slot-unbound-marker*)
                  (eq obj *illegal-marker*)
                  (eq obj *unbound-function*))
              fulltag-immediate)
             (t (error "not a CCL-VM object: ~s" obj))))))

(deftype ccl-object () `(or ccl-fixnum boolean list character single-float
                            ccl-uvector
                            (eql ,*unbound-marker*)
                            (eql ,*slot-unbound-marker*)
                            (eql ,*illegal-marker*)
                            (eql ,*unbound-function*)))

(declaim (inline ccl-object-p))
(defun ccl-object-p (obj) (typep obj 'ccl-object))

(defun lisptag (obj) ;; returns 3 bit typecode
  (logand 7 (fulltag obj)))

(defun typecode (obj)
  (let ((tag (lisptag obj)))
    (if (eq tag lisptag-misc)
      (ccl-uvector-subtag obj)
      ;; It doesn't seem to work otherwise...
      (if (eq tag lisptag-symbol)
        subtag-symbol
        (if (eq tag lisptag-function)
          subtag-function
          tag)))))

(defun ccl (obj)
  (typecase obj
    (ccl-uvector obj)
    (simple-base-string (ccl-string obj))
    ((or null ccl-fixnum single-float character) obj)
    (symbol (ccl-symbol obj))
    (integer (ccl-bignum obj))
    (number (ccl-number obj))
    (simple-vector (ccl-vector obj))
    (t (error "Don't know how to cclify ~s" obj))))
        
(defun ccl-number (obj)
  (typecase obj
    ((or ccl-fixnum single-float) obj)
    (integer (ccl-bignum obj))
    (t (error "~s conversion not implemented yet" obj))))

(defun ccl-string (obj)
  (make-ccl-simple-base-string :subtag subtag-simple-string
                               :data (coerce obj 'simple-vector)))

(defun ccl-vector (obj)
  (error "ccl-vector not implemented yet for ~s" obj))

;;;; *** TODO: another weird thing to figure out and bootstrap
(defvar %find-classes% (make-hash-table :test 'eq))

(defun find-class-cell (name create?)
  (let ((cell (gethash name %find-classes%)))
    (or cell
        (and create?
             ;; (%istruct 'class-cell name nil '%make-instance nil)
             (setf (gethash name %find-classes%)
                   (make-ccl-uvector :subtag subtag-istruct
                                     :data (vector
                                            (register-istruct-cell (ccl 'class-cell))
                                            name
                                            nil
                                            (ccl '%make-instance)
                                            nil)))))))


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

