(in-package :ccl-vm)

;; While we start up, we use this bootstrapping version of packages, until at some point
;;  in the loading, we'll turn them off and start using the native CCL packages with their
;; hash codes.  INTERN and %PKG-REF-INTERN (comes out of compiler optimizer) are in l1-symhash,
;;   They call %find-symbol and %add-symbol which are in level-0;nfasload.  So somewhere along
;; in there we need to switch the representation to one that matches CCL.

;;;; *** TODO: eventually might want to be able to reload these files without clobbering the VM,
;;;;   so should move all the startup things into a start-vm function.


(defparameter *level-0-packages* t)

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
(defmethod print-uvector-data ((type (eql :symbol)) sym stream)
  (print-uvector-data :simple-string (sym-pname sym) stream))

(defun make-ccl-symvector (pname &optional (flags 0) (value *unbound-marker*))
  (check-type pname ccl-simple-base-string)
  (%make-ccl-symvector :subtag subtag-symbol
                       :data (vector pname   ;; pname
                                     value  ;;vcell
                                     *unbound-function* ;; fcell
                                     nil ;;pkg & type predicate
                                     flags  ;; flags
                                     ()  ;; plist
                                     0))) ;; binding index

(defparameter *nil-sym* (make-ccl-symvector (ccl-string "NIL")
                                            (logior (ash 1 $sym_vbit_special) (ash 1 $sym_vbit_constant))
                                            nil))
(defparameter *t-sym* (make-ccl-symvector (ccl-string "T")
                                          (logior (ash 1 $sym_vbit_special) (ash 1 $sym_vbit_constant))
                                          T))

(defparameter *all-packages-sym*
  (make-ccl-symvector (ccl-string "%ALL-PACKAGES%") (ash 1 $sym_vbit_special) nil))


(def-uvector-subtype :package (ccl-package (:constructor %make-ccl-package) (:subtag-conser t))
  )

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
  (svref (ccl-uvector-data (sym-symvector sym)) sym.pname))

(defun sym-native-pname (sym)
  (native-string (sym-pname sym)))

(defun sym-pkg (sym)
  (setq sym (sym-symvector sym))
  (let ((pp (svref (uvector sym) sym.pkg-predicate)))
    (if (consp pp) (car pp) pp)))

;; This is defined in lispequ is is architecture-independent.
(defconstant pkg.itab 0)
(defconstant pkg.etab 1)
(defconstant pkg.used 2)
(defconstant pkg.used-by 3)
(defconstant pkg.names 4)
(defconstant pkg.shadowed 5)
(defconstant pkg.lock 6)
(defconstant pkg.intern-hook 7)

(defmethod print-uvector-data ((type (eql :package)) obj stream)
  (print-uvector-data :simple-string (car (svref (uvector obj) pkg.names)) stream))

(defun pkg-name-p (name pkg)
  (member name (svref (uvector pkg) pkg.names) :test 'uvector-equal))

(defun pkg-arg (pkg-arg &optional (error t))
  (if (ccl-package-p pkg-arg)
    pkg-arg
    (or (find pkg-arg (sym-value *all-packages-sym*) :test #'pkg-name-p)
        (and error (error "No package named ~s" pkg-arg)))))

;; COuld indirect through sym, but this is bootstrapping, so messing around with
;; %find-symbol/%add-symbol is not supported.
(defparameter *%find-symbol-func* nil)
(defparameter *%add-symbol-func* nil)

;; CCL packages are going to be too slow.  Need to intercept something and either
;; make it lap, or introduce some caching

(defun unbootstrap-packages ()
  (setq *%find-symbol-func* (sym-func (ccl '%find-symbol)))
  (setq *%add-symbol-func* (sym-func (ccl '%add-symbol)))
  (let ((resize-func (sym-func (ccl '%resize-htab))))
    (flet ((update-htab (hash)
             (when (consp hash)
               (cerror "package HTAB already updated" "ignore")
               (return-from update-htab hash))
             (let* ((count (hash-table-count hash))
                    (vec (make-array count :initial-element 0))
                    (htab (list* (make-ccl-uvector :subtag subtag-simple-vector :data vec) count count)))
               (format t "~&   hash ~s count ~s" hash count)
               (loop for sym being the hash-value of hash as index upfrom 0
                 do (setf (svref vec index) sym)
                 finally (format t " (last index ~s)" index))
               (ccl-funcall resize-func htab))))
      (loop for pkg in (sym-value *all-packages-sym*)
        do (format t "~&Updating ~s" pkg)
        do (let* ((pkg-vec (ccl-uvector-data pkg))
                  (itab (update-htab (svref pkg-vec pkg.itab)))
                  (etab (update-htab (svref pkg-vec pkg.etab))))
             (setf (svref pkg-vec pkg.itab) itab)
             (setf (svref pkg-vec pkg.etab) etab))))
    (setq *level-0-packages* nil)))

(defun find-sym-in-pkg (name pkg)
  (check-type name ccl-simple-base-string)
  (check-type pkg ccl-package)
  ;; Note with in level-0, second value is a native keyword, afterwards it's a ccl sym.
  ;; Doesn't matter because it's only used as a boolean
  (if *level-0-packages*
    (let ((hashkey (native-string name)) ;; conses, but it's just for bootstrapping, who cares.
          (pkg-vec (uvector pkg))
          (sym))
      (if (setq sym (gethash hashkey (svref pkg-vec pkg.itab)))
        (values (symvector-sym sym) :internal)
        (if (setq sym (gethash hashkey (svref pkg-vec pkg.etab)))
          (values (symvector-sym sym) :external)
          (if (setq sym (loop for p in (svref pkg-vec pkg.used)
                          thereis (gethash hashkey (svref (uvector p) pkg.etab))))
            (values (symvector-sym sym) :inherited)))))
    (ccl-funcall *%find-symbol-func* name (uvsize name) pkg)))

(defun sym-in-pkg-p (name pkg)
  (nth-value 1 (find-sym-in-pkg name pkg)))

(defun add-sym-to-pkg (sym pkg &optional (export-p nil))
  (declare (special *keyword-pkg*)) ;; defined below
  (ASSERT *LEVEL-0-PACKAGES*)
  (check-type sym ccl-symvector)
  (check-type pkg ccl-package)
  (let ((old (svref (uvector sym) sym.pkg-predicate)))
    ;; Probably don't need to support the type-predicate thing while bootstrapping?
    (if (consp old)
      (unless (car old) (setf (car old) pkg))
      (unless old (setf (svref (uvector sym) sym.pkg-predicate) pkg))))
  (let* ((sym-vec (uvector sym))
         (hashkey (sym-native-pname sym)))
    (if (eq pkg *keyword-pkg*)
      (progn
        (setf (gethash hashkey (svref (uvector pkg) pkg.etab)) sym)
        (setf (svref sym-vec sym.vcell) (symvector-sym sym))
        (setf (svref sym-vec sym.bits)
              (logior (ash 1 $sym_vbit_special)
                      (ash 1 $sym_vbit_constant)
                      (svref sym-vec sym.bits))))
      (if export-p
        (setf (gethash hashkey (svref (uvector pkg) pkg.etab)) sym)
        (setf (gethash hashkey (svref (uvector pkg) pkg.itab)) sym))))
  (assert (null (svref (uvector pkg) pkg.intern-hook)))
  sym)

(defun find-or-make-sym (name pkg &optional dont-need-to-copy-name-p)
  (if *level-0-packages*
    (multiple-value-bind (sym found-p) (find-sym-in-pkg name pkg)
      (if found-p
        sym
        (add-sym-to-pkg (make-ccl-symvector name) pkg)))
    ;; intern not defined yet, but it's basically this.
    (multiple-value-bind (sym found-p ioffs eoffs) (find-sym-in-pkg name pkg)
      (if found-p
        sym
        (ccl-funcall *%add-symbol-func*
                     (if dont-need-to-copy-name-p
                       name
                       (let ((v (ccl-uvector-data name)))
                         (ccl-string (make-array (length v) :initial-contents v))))
                     pkg ioffs eoffs)))))


;; Since we're single-threaded, there is only one value, and that is the global value!
(defun sym-boundp (sym)
  (let* ((symvec (sym-symvector sym))
         (val #+vm-threads (let ((index (svref (uvector symvec) sym.binding-index)))
                             (if (and (< index (length *thread-local-special-bindings-vector*))
                                      (not (eq *no-thread-local-binding-marker*
                                               (aref *thread-local-special-bindings-vector* index))))
                               (aref *thread-local-special-bindings-vector* index)
                               (svref (uvector symvec) sym.vcell)))
              #-vm-threads (svref (uvector symvec) sym.vcell)))
    (not (eq val *unbound-marker*))))

;; Since we're single-threaded, there is only one value, and that is the global value!
(defun sym-value (sym)
  (let* ((symvec (sym-symvector sym))
         (val #+vm-threads (let ((index (svref (uvector symvec) sym.binding-index)))
                             (if (and (< index (length *level-0-special-bindings-vector*))
                                      (not (eq *no-thread-local-binding-marker*
                                               (aref *level-0-special-bindings-vector* index))))
                               (aref *level-0-special-bindings-vector* index)
                               (svref (uvector symvec) sym.vcell)))
              #-vm-threads (svref (uvector symvec) sym.vcell)))
    (if (eq val *unbound-marker*)
      (error "Unbound variable ~s" sym)
      val)))

;; spentry(specset)  (ed "ccl:lisp-kernel;x86-spentry64.s")
;; Since we're single-threaded, there is only one value, and that is the global value!
(defun (setf sym-value) (val sym)
  (typecode val) ;; check that a ccl object
  (assert (not (eq val *unbound-marker*)))
  #+vm-threads (let* ((symvec (sym-symvector sym))
                      (index (svref (uvector symvec) sym.binding-index)))
                 (if (and (< index (length *special-bindings-vector*))
                          (not (eq *no-thread-local-binding-marker*
                                   (aref *special-bindings-vector* index))))
                   (setf (aref *special-bindings-vector* index) val)
                   (setf (svref (uvector symvec) sym.vcell) val)))
  #-vm-threads (let* ((symvec (sym-symvector sym)))
                 (setf (svref (uvector symvec) sym.vcell) val)))

(defun sym-fboundp (sym)
  (let* ((symvec (sym-symvector sym))
         (fn (svref (uvector symvec) sym.fcell)))
    (unless (eq fn *unbound-function*)
      fn)))
    
(defun sym-func (sym)
  (let* ((symvec (sym-symvector sym))
         (fn (svref (uvector symvec) sym.fcell)))
    (if (eq fn *unbound-function*)
      (error "Unfbound variable ~s" sym)
      fn)))

;; %fhave.  Doesn't check the value, so can use it to set macros and such
(defun (setf sym-func) (val sym)
  (let* ((symvec (sym-symvector sym)))
    (setf (svref (uvector symvec) sym.fcell) val)))

#+vm-threads
(defun ensure-binding-index (sym)
  (let* ((symvec (sym-symvector sym))
         (index (svref (uvector symvec) sym.binding-index))
         (bits (svref (uvector symvec) sym.bits)))
    (if (or (logbitp $sym_vbit_global bits)      ;; globals don't need binding index.
            (logbitp $sym_vbit_constant bits))
      (unless (zerop index)
        (setf (aref *special-bindings-vector* index) *no-thread-local-binding-marker*)
        (setf (svref (uvector sym) sym.binding-index) 0))
      (when (zerop index)
        (setf (svref (uvector symvec) sym.binding-index)
              (vector-push-extend *no-thread-local-binding-marker*
                                  *special-bindings-vector*
                                  100))))))

;; While bootstrapping, itab and etab are native hash tables. Everything else is real.
(defun initial-pkg (names use)
  (assert *level-0-packages*)
  (let* ((pkg-vec (vector
                   (make-hash-table :test 'equal) ;; itab
                   (make-hash-table :test 'equal) ;; etab
                   () ;; used
                   ()  ;; used-by
                   (mapcar #'ccl-string names) ;; names
                   ()  ;; shadowed
                   nil ;; lock, don't need it.
                   nil ;; intern-hook
                   ))
         (pkg (%make-ccl-package :subtag subtag-package :data pkg-vec))
         (pkgs-to-use (mapcar #'(lambda (s) (pkg-arg (ccl-string s))) use))
         (added nil)
         (done nil))
    (unwind-protect
        (loop for other in pkgs-to-use
          do (push other (svref pkg-vec pkg.used))
          do (let ((other-vec (ccl-uvector-data other)))
               (push other-vec added)
               (push pkg (svref other-vec pkg.used-by)))
          finally (setq done t))
      (if done
        (push pkg (sym-value *all-packages-sym*))
        (loop for other-vec in added
          do (setf (svref other-vec pkg.used-by)
                   (remove pkg (svref other-vec pkg.used-by))))))
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
  (let ((pname (ccl-string (symbol-name native-sym))))
    (assert (not (sym-in-pkg-p pname *cl-pkg*)))
    (add-sym-to-pkg (if (null native-sym) *nil-sym*
                      (if (eq native-sym t) *t-sym*
                        (make-ccl-symvector pname)))
                    *cl-pkg*
                    t)))

(add-sym-to-pkg *all-packages-sym* *ccl-pkg*)

;; called for fasloading and also runtime.  Should be pretty similar to the actual
;; %defconstant/%defvar/%defparameter, since will keep getting called for fasloaded functions even after bootstrap.
;; Or maybe should replace...
(defun %defconstant (sym val &optional doc)
  (%defvar sym doc 'constant val)
  (let* ((vec (uvector sym)))
    (setf (svref vec sym.bits)
          (logior (ash 1 $sym_vbit_constant)
                  (svref vec sym.bits)))))

(defun %defvar (sym doc def-type &optional (val nil val-p))
  (check-type sym ccl-symvector)
  (record-debug-info sym doc def-type)
  (let* ((vec (uvector sym)))
    (setf (svref vec sym.bits)
          (logior (ash 1 $sym_vbit_special)
                  (svref vec sym.bits))))
  (when val-p
    (setf (sym-value sym) val)))


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

(defun sym-keyword (sym)
  (assert (eq (sym-pkg sym) *keyword-pkg*))
  (intern (sym-native-pname sym) :keyword))

