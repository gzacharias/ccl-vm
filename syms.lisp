(in-package :ccl-vm)

;; While we start up, we use this bootstrapping version of packages, until at some point
;;  in the loading, we'll turn them off and start using the native CCL packages with their
;; hash codes.  INTERN and %PKG-REF-INTERN (comes out of compiler optimizer) are in l1-symhash,
;;   They call %find-symbol and %add-symbol which are in level-0;nfasload.  So somewhere along
;; in there we need to switch the representation to one that matches CCL.

;;;; *** TODO: eventually might want to be able to reload these files without clobbering the VM,
;;;;   so should move all the startup things into a start-vm function.


;; This is defined in x8664-arch, but there is plenty of code around that accesses
;; it as target::xxx, so really need all these slots to be there.
(defconstant sym.pname 0)
(defconstant sym.vcell 1)
(defconstant sym.fcell 2)
(defconstant sym.pkg-predicate 3)
(defconstant sym.bits 4)
(defconstant sym.plist 5)
;(defconstant sym.binding-index 6) ;; not used since we're single-threaded
(defconstant sym.size 7)

(defconstant $sym_vbit_bound 0)		;Proclaimed bound.
(defconstant $sym_vbit_constant 1)
(defconstant $sym_vbit_global 2)         ;Should never be lambda-bound.
(defconstant $sym_vbit_special 4)
(defconstant $sym_vbit_typeppred 5)
(defconstant $sym_vbit_indirect 6)
(defconstant $sym_vbit_defunct 7)
(defconstant $sym_fbit_frozen (+ 8 $sym_vbit_bound))
(defconstant $sym_fbit_special (+ 8 $sym_vbit_special))
(defconstant $sym_fbit_indirect (+ 8 $sym_vbit_indirect))
(defconstant $sym_fbit_defunct (+ 8 $sym_vbit_defunct))
(defconstant $sym_fbit_constant_fold (+ 8 $sym_vbit_constant))
(defconstant $sym_fbit_fold_subforms (+ 8 $sym_vbit_global))

;;; ** TODO: rename subtag-symbol to subtag-symvector
(defmethod print-object ((sym ccl-symvector) stream)
  (assert (eq (uvector-subtag sym) subtag-symbol))
  (format stream "<")
  (print-symbol-data sym stream)
  (format stream ">"))

(defmethod print-uvector-data ((type (eql :symbol)) sym stream) (print-symbol-data sym stream))

(defun print-symbol-data (sym stream)
  (setq sym (sym-symvector sym))
  (let ((pkg (sym-pkg sym)))
    (if (eq pkg *cl-pkg*)
      (format stream "CL:~a" (sym-native-pname sym))
      (if (eq pkg *ccl-pkg*)
        (format stream "CCL::~a" (sym-native-pname sym))
        (if (eq pkg *keyword-pkg*)
          (format stream ":~a" (sym-native-pname sym))
          (if (null pkg)
            (format stream "#:~a" (sym-native-pname sym))
            (format stream "~a::~s" (native-string (pkg-name (sym-pkg sym))) (sym-native-pname sym))))))))


(defun make-ccl-symvector (pname &optional (flags 0) (value *unbound-marker*))
  (check-type pname ccl-simple-string)
  (%make-ccl-symvector :subtag subtag-symbol
                       :data (vector pname   ;; pname
                                     value  ;;vcell
                                     *unbound-function* ;; fcell
                                     nil ;;pkg & type predicate
                                     flags  ;; flags
                                     ()  ;; plist
                                     0))) ;; binding index

;; Early symbols, will get interned once packages are set up.
(defvar *early-ccl-syms* nil)

(defmacro def-early-sym (var pname &rest inits)
  `(progn
     (defparameter ,var (make-ccl-symvector (ccl-string ,pname) ,@inits))
     (push (cons ,pname ,var) *early-ccl-syms*)
     ',var))

(def-early-sym *nil-sym* "NIL" (logior (ash 1 $sym_vbit_special) (ash 1 $sym_vbit_constant)) nil)
(def-early-sym *t-sym* "T" (logior (ash 1 $sym_vbit_special) (ash 1 $sym_vbit_constant)) T)

(declaim (inline sym-symvector symvector-sym))
(defun sym-symvector (sym)
  (if (null sym) *nil-sym*
    (if (eq sym t) *t-sym*
      sym)))

(defun symvector-sym (symvector)
  (if (eq symvector *nil-sym*) nil
    (if (eq symvector *t-sym*) t
      symvector)))

(defun sym-pname (sym)
  (gvref (sym-symvector sym) sym.pname))

(defun sym-native-pname (sym)
  (native-string (sym-pname sym)))

;; Since we're single-threaded, there is only one value, and that is the global value!
(defun %symptr-value (symvec)
  #+vm-threads (let ((index (gvref symvec sym.binding-index)))
                 (if (and (< index (length *level-0-special-bindings-vector*))
                          (not (eq *no-thread-local-binding-marker*
                                   (aref *level-0-special-bindings-vector* index))))
                   (svref *level-0-special-bindings-vector* index)
                   (gvref symvec sym.vcell)))
  #-vm-threads (gvref symvec sym.vcell))

(defun %set-symptr-value (symvec value)
  #+vm-threads (let* ((index (gvref symvec sym.binding-index)))
                 (if (and (< index (length *special-bindings-vector*))
                          (not (eq *no-thread-local-binding-marker*
                                   (aref *special-bindings-vector* index))))
                   (setf (aref *special-bindings-vector* index) value)
                   (setf (gvref symvec sym.vcell) value)))
  #-vm-threads (setf (gvref symvec sym.vcell) value))

(defun sym-boundp (sym)
  (not (eq (%symptr-value (sym-symvector sym)) *unbound-marker*)))

(defun sym-value (sym)
  (let ((val (%symptr-value (sym-symvector sym))))
    (if (eq val *unbound-marker*)
      (error "Unbound variable ~s" sym)
      val)))


(defun (setf sym-value) (value sym)
  (check-type value ccl-object)
  (assert (not (eq value *unbound-marker*)))
  (%set-symptr-value (sym-symvector sym) value))


;; called for fasloading and also runtime.  Should be pretty similar to the actual
;; %defconstant/%defvar/%defparameter, since will keep getting called for fasloaded functions even after bootstrap.
;; Or maybe should replace...

(defun %defvar (sym doc def-type &optional (val nil val-p))
  (check-type sym ccl-symvector)
  (record-debug-info sym doc def-type)
  (let* ((vec (gvector-data sym)))
    (setf (svref vec sym.bits)
          (logior (ash 1 $sym_vbit_special)
                  (svref vec sym.bits))))
  (when val-p
    (setf (sym-value sym) val)))

(defun %defconstant (sym val &optional doc)
  (%defvar sym doc 'constant val)
  (let* ((vec (gvector-data sym)))
    (setf (svref vec sym.bits)
          (logior (ash 1 $sym_vbit_constant)
                  (svref vec sym.bits)))))



(defun sym-fboundp (sym)
  (let* ((symvec (sym-symvector sym))
         (fn (gvref symvec sym.fcell)))
    (unless (eq fn *unbound-function*)
      fn)))
    
(defun sym-func (sym)
  (let* ((symvec (sym-symvector sym))
         (fn (gvref symvec sym.fcell)))
    (if (eq fn *unbound-function*)
      (error "Unfbound variable ~s" sym)
      fn)))

;; %fhave.  Doesn't check the value, so can use it to set macros and such
(defun (setf sym-func) (val sym)
  (let* ((symvec (sym-symvector sym)))
    (setf (gvref symvec sym.fcell) val)))

#+vm-threads
(defun ensure-binding-index (sym)
  (let* ((symvec (sym-symvector sym))
         (index (gvref symvec sym.binding-index))
         (bits (gvref symvec sym.bits)))
    (if (or (logbitp $sym_vbit_global bits)      ;; globals don't need binding index.
            (logbitp $sym_vbit_constant bits))
      (unless (zerop index)
        (setf (aref *special-bindings-vector* index) *no-thread-local-binding-marker*)
        (setf (gvref sym sym.binding-index) 0))
      (when (zerop index)
        (setf (gvref symvec sym.binding-index)
              (vector-push-extend *no-thread-local-binding-marker*
                                  *special-bindings-vector*
                                  100))))))

(defun sym-pkg (sym)
  (setq sym (sym-symvector sym))
  (let ((pp (gvref sym sym.pkg-predicate)))
    (if (consp pp) (car pp) pp)))


(def-uvector-subtype :package (ccl-package (:constructor %make-ccl-package) (:subtag-conser t)))

;; This is defined in lispequ is is architecture-independent.
(defconstant pkg.itab 0)
(defconstant pkg.etab 1)
(defconstant pkg.used 2)
(defconstant pkg.used-by 3)
(defconstant pkg.names 4)
(defconstant pkg.shadowed 5)
(defconstant pkg.lock 6)
(defconstant pkg.intern-hook 7)

(defun %new-htab (size)
  (list* (make-uvector subtag-simple-vector (make-array size)) 0 (make-hash-table :test 'equal)))

(defun %itab-get (hashkey pkg-vec)
  (gethash hashkey (cddr (svref pkg-vec pkg.itab))))

(defun %etab-get (hashkey pkg-vec)
  (gethash hashkey (cddr (svref pkg-vec pkg.etab))))

(defun %htab-add (hashkey htab sym)
  (destructuring-bind (uvec count . hash) htab
    (let ((vec (gvector-data uvec)))
      (when (eql count (length vec))
        (setf (uvector-data uvec)
              (setq vec (adjust-array vec  (+ count 100) :initial-element 0))))
      (let ((newpos (position 0 vec)))
        (assert newpos)
        (setf (svref vec newpos) sym)
        (setf (cadr htab) (1+ count)))
      (setf (gethash hashkey hash) sym))))

(defun %itab-add (hashkey pkg-vec sym)
  (%htab-add hashkey (svref pkg-vec pkg.itab) sym))

(defun %etab-add (hashkey pkg-vec sym)
  (%htab-add hashkey (svref pkg-vec pkg.itab) sym))

(defun %htab-rem (hashkey htab sym)
  (destructuring-bind (uvec count . hash) htab
    ;; The vector is there for iteration, so don't shift its contents around.
    (let* ((vec (gvector-data uvec))
           (sympos (position sym vec)))
      (if (not sympos) ;; shouldn't happen but at least make sure we're consistent
        (assert (not (gethash hashkey hash)))
        (progn
          (setf (svref vec sympos) 0)
          (setf (cadr htab) (1- count))
          (remhash hashkey hash))))))

(defun %itab-rem (hashkey pkg-vec sym)
  (%htab-rem hashkey (svref pkg-vec pkg.itab) sym))

(defun %etab-rem (hashkey pkg-vec sym)
  (%htab-rem hashkey (svref pkg-vec pkg.etab) sym))


(defmethod print-uvector-data ((type (eql :package)) obj stream) (print-package-data obj stream))

(defun print-package-data (obj stream)
  (print-string-data (pkg-name obj) stream))

(defun pkg-name (pkg)
  (car (gvref pkg pkg.names)))

(defun pkg-name-p (name pkg)
  (member name (gvref pkg pkg.names) :test #'uvector-equal))


(def-early-sym *all-packages-sym* "%ALL-PACKAGES%" (ash 1 $sym_vbit_special) nil)
(def-early-sym *all-packages-lock-sym* "%ALL-PACKAGES-LOCK%" (ash 1 $sym_vbit_special) nil)


;; Need this to make package-ref's
(def-early-sym *istruct-cells-sym* "*ISTRUCT-CELLS*" (ash 1 $sym_vbit_special) nil)
(defun register-istruct-cell (sym)
  ;; Could switch to use ccl register-istruct-cells once it's defined, but don't bother, it's not changing.
  ;; (if (fboundp (ccl'register-istruct-cell)) (ccl-funcall (ccl'register-istruct-cell) sym) ...)
  (let ((alist (sym-value *istruct-cells-sym*)))
    (or (assoc sym alist)
        (let ((pair (cons sym nil)))
          (setf (sym-value *istruct-cells-sym*) (cons pair alist))
          pair))))



(defun pkg-arg (pkg-arg &optional (errorp t))
  (cond ((ccl-package-p pkg-arg)
         (unless (gvref pkg-arg pkg.names)
           (error "Package ~s is deleted" pkg-arg))
         pkg-arg)
        (t
         (when (typep pkg-arg 'ccl-symbol)
           (setq pkg-arg (sym-pname pkg-arg)))
         ;; should allow non-simple-strings         
         ; (setq pkg-arg (ensure-simple-string pkg-arg))
         (check-type pkg-arg ccl-simple-string)
         (let* ((nicknames-fn (sym-fboundp (ccl 'package-%local-nicknames)))
                (local-nicknames (and nicknames-fn
                                      (ccl-funcall nicknames-fn (sym-value (ccl '*package*))))))
           (or (cdr (assoc pkg-arg local-nicknames :test #'uvector-equal))
               (or (find pkg-arg (sym-value *all-packages-sym*) :test #'pkg-name-p)
                   (and errorp (error "No package named ~s" pkg-arg))))))))

(def-early-sym *package-ref-sym* "PACKAGE-REF")

(defparameter *package-refs* ())

(defun register-package-ref (name pkg) ;; pkg may be nil
  (let* ((ref (cdr (or (assoc name *package-refs* :test #'uvector-equal)
                       (car (setq *package-refs*
                                  (cons (cons name (make-istruct *package-ref-sym* name nil))
                                        *package-refs*))))))
         (vec (gvector-data ref)))
    (or (svref vec 2)
        (setf (svref vec 2) pkg))
    ref))

(defun find-sym-in-pkg (name pkg)
  (check-type name ccl-simple-string)
  (check-type pkg ccl-package)
  (let ((hashkey (native-string name)) ;; conses, but it's just for bootstrapping, who cares.
        (pkg-vec (gvector-data pkg))
        (sym))
    (if (setq sym (%itab-get hashkey pkg-vec))
      (values (symvector-sym sym) :internal)
      (if (setq sym (%etab-get hashkey pkg-vec))
        (values (symvector-sym sym) :external)
        (if (setq sym (loop for p in (svref pkg-vec pkg.used)
                        thereis (%etab-get hashkey (gvector-data p))))
          (values (symvector-sym sym) :inherited))))))

(defun sym-in-pkg-p (name pkg)
  (nth-value 1 (find-sym-in-pkg name pkg)))


(defun add-sym-to-pkg (sym pkg &optional (export-p nil))
  (declare (special *keyword-pkg*))
  (check-type sym ccl-symvector)
  (check-type pkg ccl-package)
  (let ((old (svref (gvector-data sym) sym.pkg-predicate)))
    (if (consp old)
      (unless (car old) (setf (car old) pkg))
      (unless old (setf (svref (gvector-data sym) sym.pkg-predicate) pkg))))
  (let* ((hashkey (sym-native-pname sym)))
    (if (eq pkg *keyword-pkg*)
      (let ((sym-vec (gvector-data sym)))
        (%etab-add hashkey (gvector-data pkg) sym)
        (setf (svref sym-vec sym.vcell) (symvector-sym sym))
        (setf (svref sym-vec sym.bits)
              (logior (ash 1 $sym_vbit_special)
                      (ash 1 $sym_vbit_constant)
                      (svref sym-vec sym.bits))))
      (if export-p ; (OR FORCE-EXPORT-PACKAGE-p) - used in objc-bridge only.
        (%etab-add hashkey (gvector-data pkg) sym)
        (%itab-add hashkey (gvector-data pkg) sym))))
  (assert (null (svref (gvector-data pkg) pkg.intern-hook)))
  sym)

(defun export-sym-from-pkg (sym pkg)
  (check-type sym ccl-symvector)
  (let* ((hashkey (sym-native-pname sym))
         (pkg-vec (gvector-data pkg))
         (foundsym (%itab-get hashkey pkg-vec)))
    (when foundsym
      (assert (eq foundsym sym))
      (%itab-rem hashkey (gvector-data pkg) sym))
    (if (setq foundsym (%etab-get hashkey pkg-vec))
      (assert (eq foundsym sym))
      (%etab-add hashkey (gvector-data pkg) sym))))

(defun find-or-make-sym (name pkg)
  (multiple-value-bind (sym found-p) (find-sym-in-pkg name pkg)
    (if found-p
      sym
      (add-sym-to-pkg (make-ccl-symvector name) pkg))))

(defun initial-pkg (native-names use)
  (let* ((names (mapcar #'ccl-string native-names))
         (pkg-vec (vector
                   (%new-htab 0) ;; itab
                   (%new-htab 0) ;; etab
                   () ;; used
                   ()  ;; used-by
                   names ;; names
                   ()  ;; shadowed
                   nil ;;u lock - will get added by l0-aprims
                   nil ;; intern-hook
                   ))
         (pkg (%make-ccl-package :subtag subtag-package :data pkg-vec))
         (pkgs-to-use (mapcar #'(lambda (s)
                                  (or (find (ccl-string s) (sym-value *all-packages-sym*) :test #'pkg-name-p)
                                      (error "No initial package named ~s" s)))
                              use))
         (added nil)
         (done nil))
    (unwind-protect
        (loop for other in pkgs-to-use
          do (push other (svref pkg-vec pkg.used))
          do (let ((other-vec (gvector-data other)))
               (push other-vec added)
               (push pkg (svref other-vec pkg.used-by)))
          finally (setq done t))
      (if done
        (push pkg (sym-value *all-packages-sym*))
        (loop for other-vec in added
          do (setf (svref other-vec pkg.used-by)
                   (remove pkg (svref other-vec pkg.used-by))))))
    (dolist (name names)
      (register-package-ref name pkg))
    pkg))

(defparameter *cl-pkg*      (initial-pkg '("COMMON-LISP" "CL") ()))
(defparameter *keyword-pkg* (initial-pkg '("KEYWORD") ()))
(defparameter *ccl-pkg*     (initial-pkg '("CCL") '("COMMON-LISP")))
(defparameter *target-pkg*  (initial-pkg '("CVM" "TARGET") '("COMMON-LISP")))
(defparameter *os-pkg*      (initial-pkg '("CVM-DARWIN64" "OS") '("COMMON-LISP")))
(defparameter *ffi-pkg*     (initial-pkg '("CVMDARWIN-FFI") ()))

;; Initialize the COMMON-LISP package..  Assume our host is compliant and just copy theirs.
;; Note this doesn't set up flags, that should happen as we load.
(do-external-symbols (native-sym :common-lisp)
  (let* ((native-pname (symbol-name native-sym))
         (pname (ccl-string native-pname)))
    (assert (not (sym-in-pkg-p pname *cl-pkg*)))
    (add-sym-to-pkg (let ((early (assoc native-pname *early-ccl-syms* :test 'equal)))
                      (or (when early
                            (setq *early-ccl-syms* (remove early *early-ccl-syms*))
                            (cdr early))
                          (make-ccl-symvector pname)))
                    *cl-pkg*
                    t)))

(loop while *early-ccl-syms*
  for (nil . sym)  = (pop *early-ccl-syms*)
  do (add-sym-to-pkg sym *ccl-pkg*)
  finally (makunbound '*early-ccl-syms*))


(defparameter *native-package* (symbol-package '*native-package*))

(defun ccl-symbol (symbol)
  (if (typep symbol 'ccl-symbol) ;; note this includes nil and T
    symbol
    (progn
      (check-type symbol symbol)
      (let* ((native-name (symbol-name symbol))
             (name (ccl-string native-name)))
        (if (keywordp symbol)
          (find-or-make-sym name *keyword-pkg*)
          (if (eq (find-symbol native-name :common-lisp) symbol)
            (or (find-sym-in-pkg name *cl-pkg*)
                (error "Unknown CL symbol ~s" symbol))
            (progn
              (assert (eq (symbol-package symbol) *native-package*))
              (find-or-make-sym name *ccl-pkg*))))))))

(defun native-symbol (sym)
  (check-type sym ccl-symbol)
  (let* ((pname (sym-native-pname sym))
         (pkg (sym-pkg sym)))
    (cond ((eq pkg *cl-pkg*) (intern pname :common-lisp))
          ((eq pkg *keyword-pkg*) (intern pname :keyword))
          ((eq pkg *ccl-pkg*) (intern pname *native-package*))
          ((eq pkg *ffi-pkg*) (intern pname :ccl-ffi))
          (t (error "Don't know how to nativize ~s" sym)))))

(defun sym-keyword (sym)
  (assert (eq (sym-pkg sym) *keyword-pkg*))
  (intern (sym-native-pname sym) :keyword))


