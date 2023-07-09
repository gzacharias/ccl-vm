(in-package :ccl-vm)

;; While we start up, we use this bootstrapping version of packages, until at some point
;;  in the loading, we'll turn them off and start using the native CCL packages with their
;; hash codes.  INTERN and %PKG-REF-INTERN (comes out of compiler optimizer) are in l1-symhash,
;;   They call %find-symbol and %add-symbol which are in level-0;nfasload.  So somewhere along
;; in there we need to switch the representation to one that matches CCL.

;; This is defined in x8664-arch, but there is plenty of code around that accesses
;; it as target::xxx, so really need all these slots to be there.
(defconstant sym.pname 0)
(defconstant sym.vcell 1)
(defconstant sym.fcell 2)
(defconstant sym.pkg-predicate 3)
(defconstant sym.bits 4)
(defconstant sym.plist 5)
(defconstant sym.binding-index 6)
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
  (print-uvector-data :simple-string (svref (uvector sym) sym.pname) stream))

(defun make-ccl-symvector (pname)
  (check-type pname ccl-simple-base-string)
  (%make-ccl-symvector :subtag subtag-symbol
                       :data (vector pname   ;; pname
                                     *unbound-marker*  ;;vcell
                                     *unbound-function* ;; fcell
                                     nil ;;pkg & type predicate
                                     0  ;; flags
                                     ()  ;; plist
                                     0))) ;; binding index

(defparameter *nil-sym* (make-ccl-symvector (ccl-string "NIL")))
(defparameter *t-sym* (make-ccl-symvector (ccl-string "T")))

(defparameter *all-packages-sym*
  (let ((sym (make-ccl-symvector (ccl "%ALL-PACKAGES%"))))
    (setf (svref (uvector sym) sym.bits) (ash 1 $sym_vbit_special))
    (setf (svref (uvector sym) sym.vcell) nil)
    sym))

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

;; While bootstrapping, itab and etab are native hash tables.  After bootstrapping,
;; this function shouldn't get used!
(defun cons-pkg ()
  (%make-ccl-package :subtag subtag-package
                     :data (vector
                            (make-hash-table :test 'equal) ;; itab
                            (make-hash-table :test 'equal);; etab
                            ()  ;; used
                            ()  ;; used-by
                            ()  ;; names
                            ()  ;; shadowed
                            NIL ;; lock, don't need it.
                            nil ;; intern-hook
                            )))

(defun pkg-name-p (name pkg)
  (member name (svref (uvector pkg) pkg.names) :test 'ccl-equal))

(defun pkg-arg (pkg-arg &optional (error t))
  (if (ccl-package-p pkg-arg)
    pkg-arg
    (or (find pkg-arg (sym-value *all-packages-sym*) :test #'pkg-name-p)
        (and error (error "No package named ~s" pkg-arg)))))

(defun make-pkg (&key names use)
  (init-pkg (cons-pkg) names use))

(defun init-pkg (pkg names use)
  (check-type pkg ccl-package)
  (assert (every #'ccl-simple-base-string-p names))
  (assert (not (find pkg (sym-value *all-packages-sym*))))
  (let ((pkg-vec (uvector pkg))
        (pkgs-to-use (mapcar #'pkg-arg use))
        (added nil)
        (done nil))
    (setf (svref pkg-vec pkg.names) (copy-list names))
    (unwind-protect
        (loop for other in pkgs-to-use
          do (push other (svref pkg-vec pkg.used))
          do (let ((other-vec (uvector other)))
               (push other-vec added)
               (push pkg (svref other-vec pkg.used-by)))
          finally (setq done t))
      (if done
        (push pkg (sym-value *all-packages-sym*))
        (loop for other-vec in added
          do (setf (svref other-vec pkg.used-by)
                   (remove pkg (svref other-vec pkg.used-by))))))
    pkg))



(defun find-sym-in-pkg (name pkg)
  (check-type name ccl-simple-base-string)
  (check-type pkg ccl-package)
  (let ((hashkey (coerce (uvector name) 'string)) ;; yeah, but it's just for bootstrapping, who cares.
        (pkg-vec (uvector pkg))
        (sym))
    (if (setq sym (gethash hashkey (svref pkg-vec pkg.itab)))
      (values (symvector-sym sym) :internal)
      (if (setq sym (gethash hashkey (svref pkg-vec pkg.etab)))
        (values (symvector-sym sym) :external)
        (if (setq sym (loop for p in (svref pkg-vec pkg.used)
                        thereis (gethash hashkey (svref (uvector p) pkg.etab))))
          (values (symvector-sym sym) :inherited))))))

(defun sym-in-pkg-p (name pkg)
  (nth-value 1 (find-sym-in-pkg name pkg)))

(defun sym-pkg (sym)
  (setq sym (sym-symvector sym))
  (let ((pp (svref (uvector sym) sym.pkg-predicate)))
    (if (consp pp) (car pp) pp)))

(defun add-sym-to-pkg (sym pkg)
  (check-type sym ccl-symvector)
  (check-type pkg ccl-package)
  (let ((old (svref (uvector sym) sym.pkg-predicate)))
    ;; Probably don't need to support the type-predicate thing while bootstrapping?
    (if (consp old)
      (unless (car old) (setf (car old) pkg))
      (unless old (setf (svref (uvector sym) sym.pkg-predicate) pkg))))
  (let* ((sym-vec (uvector sym))
         (hashkey (coerce (uvector (svref sym-vec sym.pname)) 'string)))
    (if (eq pkg *keyword-pkg*)
      (progn
        (setf (gethash hashkey (svref (uvector pkg) pkg.etab)) sym)
        (setf (svref sym-vec sym.vcell) (symvector-sym sym))
        (setf (svref sym-vec sym.bits)
              (logior (ash 1 $sym_vbit_special)
                      (ash 1 $sym_vbit_constant)
                      (svref sym-vec sym.bits))))
      (setf (gethash hashkey (svref (uvector pkg) pkg.itab)) sym)))
  (assert (null (svref (uvector pkg) pkg.intern-hook)))
  sym)

(defun find-or-make-sym (name pkg)
  (multiple-value-bind (sym found-p) (find-sym-in-pkg name pkg)
    (if found-p
      sym
      (add-sym-to-pkg (make-ccl-symvector name) pkg))))

;;;; BINDING INDEX: in L0-symbol, defined within a lexical var cloak,
;;;   in l0, so it's in the initial image
;;;; ENSURE-BINDING-INDEX looks at symbol,
;;;  if it's either global or const, then clear the binding index.
;;;  otherwise, give it a binding index if it doesn't have one.
;;;;  There is a hash table mapping INDEX <-> SYMBOL
;;;;  In the cold load, assign binding indices, then at end of %toplevel-function%  MAP OVER MEMORY!!!
;;;;    to find all symbols and set the reverse mapping!  CHANGE THAT SO COLD-LOAD MAKES A LIST OF THE SYMBOLS.

;;; **
(defvar *bootstrapping-binding-index* 0)
(defvar *bootstrapping-binding-index-vars* (make-hash-table :test #'eql))

;; TO switch to CCL maintaining the binding index, maphash and call #CCL:cold-load-binding-index
;; on all the syms
;; In L0-syms, so need to switch over somehow once L0-symbol is loaded.
;;  I think this needs to be done by having an interim definition for things in the VM...

;;; MAKE CCL-SYMBOL be a union type, define (uvector nil) to return *nil-sym*'s uvector?

;; 
(defparameter *no-thread-local-binding-marker* 'no-thread-local-binding)

;; these would be per thread, if we had threads.
(defvar *special-bindings-vector* (make-array 2
                                              :adjustable t
                                              :fill-pointer 1
                                              :initial-element *no-thread-local-binding-marker*))

(defun sym-boundp (sym)
  (let* ((symvec (sym-symvector sym))
         (index (svref (uvector symvec) sym.binding-index)))
    (let ((val (if (and (< index (length *special-bindings-vector*))
                        (not (eq *no-thread-local-binding-marker*
                                 (aref *special-bindings-vector* index))))
                 (aref *special-bindings-vector* index)
                 (svref (uvector symvec) sym.vcell))))
      (not (eq val *unbound-marker*)))))

;; spentry(specrefcheck)
(defun sym-value (sym)
  (let* ((symvec (sym-symvector sym))
         (index (svref (uvector symvec) sym.binding-index)))
    (let ((val (if (and (< index (length *special-bindings-vector*))
                        (not (eq *no-thread-local-binding-marker*
                                 (aref *special-bindings-vector* index))))
                 (aref *special-bindings-vector* index)
                 (svref (uvector symvec) sym.vcell))))
      (if (eq val *unbound-marker*)
        (error "Unbound variable ~s" sym)
        val))))

;; spentry(specset)  (ed "ccl:lisp-kernel;x86-spentry64.s")
(defun (setf sym-value) (val sym)
  (typecode val) ;; check that a ccl object
  (let* ((symvec (sym-symvector sym))
         (index (svref (uvector symvec) sym.binding-index)))
    (if (and (< index (length *special-bindings-vector*))
             (not (eq *no-thread-local-binding-marker*
                      (aref *special-bindings-vector* index))))
      (setf (aref *special-bindings-vector* index) val)
      (setf (svref (uvector symvec) sym.vcell) val))))

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

(defun ensure-binding-index (sym)
  (let* ((symvec (sym-symvector sym))
         (index (svref (uvector symvec) sym.binding-index))
         (bits (svref (uvector symvec) sym.bits)))
    (if (or (logbitp $sym_vbit_global bits)
            (logbitp $sym_vbit_constant bits))
      ;; globals don't need binding index.
      (unless (zerop index)
        (setf (aref *special-bindings-vector* index) *no-thread-local-binding-marker*)
        (setf (svref (uvector sym) sym.binding-index) 0))
      (when (zerop index)
        (setf (svref (uvector symvec) sym.binding-index)
              (vector-push-extend *no-thread-local-binding-marker*
                                  *special-bindings-vector*
                                  100))))))

;; called for fasloading and also runtime.  Should be pretty similar to the actual
;; %defconstant/%defvar/%defparameter, since will keep getting called for fasloaded functions even after bootstrap.
;; Or maybe should replace...

(defun %defconstant (sym val &optional doc)
  (%defvar sym doc 'constant)
  (setf (sym-value sym) val)
  (let* ((vec (uvector sym)))
    (setf (svref vec sym.bits)
          (logior (ash 1 $sym_vbit_constant)
                  (svref vec sym.bits)))))

(defun %defvar (sym doc def-type)
  (check-type sym ccl-symbol)
  (record-debug-info sym doc def-type)
  (let* ((vec (uvector sym)))
    (setf (svref vec sym.bits)
          (logior (ash 1 $sym_vbit_special)
                  (svref vec sym.bits)))))


(defparameter *native-package* (symbol-package '*native-package*))

(defun ccl-symbol (symbol)
  (if (typep symbol 'ccl-symbol) ;; note this includes nil and T
    symbol
    (progn
      (check-type symbol symbol)
      (let* ((native-name (symbol-name symbol))
             (name (ccl-string native-name)))
        (if (eq (find-symbol native-name :common-lisp) symbol)
          (or (find-sym-in-pkg name *cl-pkg*)
              (error "Unknown CL symbol ~s" symbol))
          (progn
            (assert (eq (symbol-package symbol) *native-package*))
            (find-or-make-sym name *ccl-pkg*)))))))


;;; *** TODO: figure out the transition to native packages
(defun startup-pkg (names use)
  (make-pkg :names (mapcar #'ccl-string names)
            :use (mapcar #'ccl-string use)))

(defparameter *cl-pkg*      (startup-pkg '("COMMON-LISP" "CL") nil))
(defparameter *keyword-pkg* (startup-pkg '("KEYWORD") nil))
(defparameter *ccl-pkg*     (startup-pkg '("CCL") '("COMMON-LISP")))
(defparameter *target-pkg*  (startup-pkg '("CVM" "TARGET") '("COMMON-LISP")))
(defparameter *os-pkg*      (startup-pkg '("CVM-DARWIN64" "OS") '("COMMON-LISP")))
  ;; This is  our  own thing, part of faking of the foreign function support.
  ;; might not need it.
(defparameter *ffi-pkg* (startup-pkg '("CVMDARWIN-FFI") nil))

(add-sym-to-pkg *all-packages-sym* *ccl-pkg*)

;; Initialize the COMMON-LISP package..  Assume our host is compliant and just copy theirs.
;; Note this doesn't set up flags, that should happen as we load.
(do-external-symbols (native-sym :common-lisp)
  (let ((pname (ccl-string (symbol-name native-sym))))
    (assert (not (sym-in-pkg-p pname *cl-pkg*)))
    (add-sym-to-pkg (if (null native-sym) *nil-sym*
                      (if (eq native-sym t) *t-sym*
                        (make-ccl-symvector pname)))
                    *cl-pkg*)))

