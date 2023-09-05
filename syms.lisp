(in-package :ccl-vm)

;;;; *** TODO: eventually might want to be able to reload these files without clobbering the VM,
;;;;   so should move all the startup things into a start-vm function.


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

(defvar-typed *cl-pkg* ccl-package)
(defvar-typed *keyword-pkg* ccl-package)
(defvar-typed *ccl-pkg* ccl-package)
(defvar-typed *target-pkg* ccl-package)
(defvar-typed *os-pkg* ccl-package)
(defvar-typed *ffi-pkg* ccl-package)


(defun make-ccl-symvector (pname &optional (flags 0) (value *unbound-marker*))
  (check-type pname ccl-simple-string)
  (%make-ccl-symvector :subtag subtag-symvector
                       :data (vector pname   ;; pname
                                     value  ;;vcell
                                     *unbound-function* ;; fcell
                                     nil ;;pkg & type predicate
                                     flags  ;; flags
                                     ()  ;; plist
                                     0))) ;; binding index. 0 means global

;; Early symbols, will get interned once packages are set up.
(defvar *early-ccl-syms* nil)

(defmacro def-early-sym (var pname &rest flags-and-value)
  `(progn
     (push (cons (make-ccl-symvector (ccl-string ,pname) ,@flags-and-value)  ',var) *early-ccl-syms*)
     (defvar-typed ,var ccl-symvector)))


(def-early-sym *nil-sym* "NIL" (logior (ash 1 $sym_vbit_special) (ash 1 $sym_vbit_constant)) nil)
(def-early-sym *t-sym* "T" (logior (ash 1 $sym_vbit_special) (ash 1 $sym_vbit_constant)) T)

(defun-inline sym-symvector (sym)
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

(def-uvector-print-text :symvector sym-print-text (sym)
  (setq sym (sym-symvector sym))
  (let ((pkg (sym-pkg sym)))
    (if (eq pkg *cl-pkg*)
      (format nil "CL:~a" (sym-native-pname sym))
      (if (eq pkg *ccl-pkg*)
        (format nil "CCL::~a" (sym-native-pname sym))
        (if (eq pkg *keyword-pkg*)
          (format nil ":~a" (sym-native-pname sym))
          (if (null pkg)
            (format nil "#:~a" (sym-native-pname sym))
            (format nil "~a::~s" (native-string (pkg-name (sym-pkg sym))) (sym-native-pname sym))))))))


;; Since we're single-threaded, there is only one value, and that is the global value!
(defun %sym-value (sym) (gvref (sym-symvector sym) sym.vcell))
(defun %set-sym-value (sym value) (setf (gvref (sym-symvector sym) sym.vcell) value))

(defun sym-boundp (sym)
  (not (eq (%sym-value sym) *unbound-marker*)))

(defun sym-value (sym)
  (let ((val (%sym-value sym)))
    (if (eq val *unbound-marker*)
      (error "Unbound variable ~s" sym)
      val)))


(defun (setf sym-value) (value sym)
  (check-type value ccl-object)
  (assert (not (eq value *unbound-marker*)))
  (%set-sym-value sym value))


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
      (progn
        (cerror "Try again" "Unfbound variable ~s" sym)
        (sym-func sym))
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

;; This is defined in lispequ, so is architecture-independent.
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

(defun %htab-get (hashkey htab)
  (gethash hashkey (cddr htab)))
  
(defun %itab-get (hashkey pkg-vec) (%htab-get hashkey (svref pkg-vec pkg.itab)))
(defun %etab-get (hashkey pkg-vec) (%htab-get hashkey (svref pkg-vec pkg.etab)))

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

(defun %itab-add (hashkey pkg-vec sym) (%htab-add hashkey (svref pkg-vec pkg.itab) sym))
(defun %etab-add (hashkey pkg-vec sym) (%htab-add hashkey (svref pkg-vec pkg.etab) sym))

(defun %htab-rem (hashkey htab sym)
  (destructuring-bind (uvec count . hash) htab
    ;; The vector is there for iteration, so don't shift its contents around.
    (let* ((vec (gvector-data uvec))
           (sympos (position (sym-symvector sym) vec)))
      (if (not sympos) ;; shouldn't happen but at least make sure we're consistent
        (assert (not (gethash hashkey hash)))
        (progn
          (setf (svref vec sympos) 0)
          (setf (cadr htab) (1- count))
          (remhash hashkey hash))))))

(defun %itab-rem (hashkey pkg-vec sym) (%htab-rem hashkey (svref pkg-vec pkg.itab) sym))
(defun %etab-rem (hashkey pkg-vec sym) (%htab-rem hashkey (svref pkg-vec pkg.etab) sym))

(def-uvector-print-text :package pkg-print-text (obj)
  (format nil "PKG ~a" (string-print-text (pkg-name obj))))

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

(defun %find-pkg (name &optional end)
  (check-type name ccl-simple-string)
  (when (and end (not (eql end (uvsize name))))
    (with-uvector-data (data name) :error
      (setq name (make-uvector subtag-simple-string (subseq data 0 end)))))
  (find name (sym-value *all-packages-sym*) :test #'pkg-name-p))

(defun pkg-arg (pkg-arg &optional (errorp t))
  (cond ((ccl-package-p pkg-arg)
         (unless (gvref pkg-arg pkg.names)
           (error "Package ~s is deleted" pkg-arg))
         pkg-arg)
        (t
         (when (typep pkg-arg 'ccl-symbol)
           (setq pkg-arg (sym-pname pkg-arg)))
         (let* ((nicknames-fn (sym-fboundp (ccl 'package-%local-nicknames)))
                (local-nicknames (and nicknames-fn
                                      (ccl-funcall nicknames-fn (sym-value (ccl '*package*))))))
           ; (setq pkg-arg (ensure-simple-string pkg-arg))
           (check-type pkg-arg ccl-simple-string)
           (or (cdr (assoc pkg-arg local-nicknames :test #'uvector-equal))
               (or (%find-pkg pkg-arg)
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

(defun %htab-hashkey (string-or-sym &optional len)
  (let ((string (if (ccl-simple-string-p string-or-sym)
                  string-or-sym
                  (sym-pname string-or-sym))))
    (with-uvector-data (data string) :error
      (coerce (if (or (null len) (eql len (length data))) data (subseq data 0 len)) 'string))))

(defun find-sym-in-pkg (name pkg)
  (check-type name ccl-simple-string)
  (check-type pkg ccl-package)
  (let ((hashkey (%htab-hashkey name))
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
  (setq sym (sym-symvector sym))
  (check-type sym ccl-symvector)
  (check-type pkg ccl-package)
  (let ((old (svref (gvector-data sym) sym.pkg-predicate)))
    (if (consp old)
      (unless (car old) (setf (car old) pkg))
      (unless old (setf (svref (gvector-data sym) sym.pkg-predicate) pkg))))
  (let* ((hashkey (%htab-hashkey sym)))
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
  (let* ((hashkey (%htab-hashkey sym))
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

(defun init-packages ()
  ;; Make sure all early sym vars are defined before running any code that might reference them.
  (loop for (symvec . var) in *early-ccl-syms* do (set var symvec))
  (flet ((initial-pkg (native-names use)
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
             pkg)))
    (setq *cl-pkg*      (initial-pkg '("COMMON-LISP" "CL") ()))
    (setq *keyword-pkg* (initial-pkg '("KEYWORD") ()))
    (setq *ccl-pkg*     (initial-pkg '("CCL") '("COMMON-LISP")))
    (setq *target-pkg*  (initial-pkg '("CVM" "TARGET") '("COMMON-LISP")))
    (setq *os-pkg*      (initial-pkg '("CVM-DARWIN64" "OS") '("COMMON-LISP")))
    (setq *ffi-pkg*     (initial-pkg '("CVMDARWIN-FFI") ())))

 ;; Initialize the COMMON-LISP package..  Assume our host is compliant and just copy theirs.
  ;; Has to happen before we start loading files with references to CL symbols in ccl package,
  ;; else get they created as ccl symbols.
  (do-external-symbols (native-sym :common-lisp)
    (let ((pname (ccl-string (symbol-name native-sym))))
      (assert (not (sym-in-pkg-p pname *cl-pkg*)))
      (add-sym-to-pkg (let ((early (assoc pname *early-ccl-syms* :test #'uvector-equal :key #'sym-pname)))
                        (or (when early
                              (setq *early-ccl-syms* (remove early *early-ccl-syms*))
                              (car early))
                            (make-ccl-symvector pname)))
                      *cl-pkg*
                      t)))
  (loop while *early-ccl-syms*
    for symvec = (car (pop *early-ccl-syms*))
    do (add-sym-to-pkg symvec *ccl-pkg*)
    finally (makunbound '*early-ccl-syms*))

  ;; So there is this weird thing:
  ;;  In ccl-export-syms, we export a bunch of symbols from CCL.
  ;;  In order to be exported from CCL, the symbols have to be present in the package.  5 of those
  ;;  symbols are actually CL symbols that are inherited by CCL. In the bootstrapping
  ;;  version, they are also present in CCL because, well, they have always been and so always will be.
  ;;  Since we create the package from scratch, we have to do it explicitly.
  (add-sym-to-pkg (find-sym-in-pkg (ccl-string "ADD-METHOD") *cl-pkg*) *ccl-pkg*)
  (add-sym-to-pkg (find-sym-in-pkg (ccl-string "COMPUTE-APPLICABLE-METHODS") *cl-pkg*) *ccl-pkg*)
  (add-sym-to-pkg (find-sym-in-pkg (ccl-string "METHOD-QUALIFIERS") *cl-pkg*) *ccl-pkg*)
  (add-sym-to-pkg (find-sym-in-pkg (ccl-string "REMOVE-METHOD") *cl-pkg*) *ccl-pkg*)
  (add-sym-to-pkg (find-sym-in-pkg (ccl-string "STYLE-WARNING") *cl-pkg*) *ccl-pkg*)

)

