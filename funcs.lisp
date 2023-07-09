(in-package :ccl-vm)

;;; TODO: change naming so these things are :funcs, make-ccl-func, ensure-func, etc.
;;; or lfun.  but not "function"


(defmethod print-uvector-data ((type (eql :function)) obj stream)
  (when (ccl-function-native-fn obj) (princ "Compiled " stream))
  (let ((name (ccl-function-name obj)))
    (if (typep name 'ccl-symbol)
      (print-uvector-data :symbol (sym-symvector name) stream)
      (print-object name stream))))

(defconstant $lfbits-nonnullenv-bit 0)
(defconstant $lfbits-keys-bit 1)
(defconstant $lfbits-numopt (byte 5 2))
(defconstant $lfbits-restv-bit 7)
(defconstant $lfbits-numreq (byte 6 8))
(defconstant $lfbits-optinit-bit 14)
(defconstant $lfbits-rest-bit 15)
(defconstant $lfbits-aok-bit 16)
(defconstant $lfbits-numinh (byte 6 17))
(defconstant $lfbits-info-bit 23)
(defconstant $lfbits-trampoline-bit 24)
;;; (defconstant $lfbits-code-coverage-bit 25)
(defconstant $lfbits-cm-bit 26)         ; combined-method
(defconstant $lfbits-nextmeth-bit 26)   ; or call-next-method with method-bit
(defconstant $lfbits-gfn-bit 27)        ; generic-function
(defconstant $lfbits-nextmeth-with-args-bit 27)   ; or call-next-method-with-args with method-bit
(defconstant $lfbits-method-bit 28)     ; method function
(defconstant $lfbits-noname-bit 29)

(defun cons-ccl-function ()
  (%make-ccl-function :subtag subtag-function
                      :data (vector nil 0)))


;; CCL assumes bits and name are stored in the "lfun-vector"
(defun ccl-function-bits (fn)
  (let ((vec (ccl-function-data fn)))
    (svref vec (1- (length vec)))))

(defun (setf ccl-function-bits) (val fn)
  (let ((vec (ccl-function-data fn)))
    (setf (svref vec (1- (length vec))) val)))

(defun ccl-function-name (fn)
  (let* ((vec (ccl-function-data fn))
         (last (1- (length vec)))
         (bits (svref vec last)))
    (if (logbitp $lfbits-noname-bit bits)
      nil
      (svref vec (1- last)))))

(defun (setf ccl-function-name) (val fn)
  (let* ((vec (ccl-function-data fn))
         (last (1- (length vec)))
         (bits (svref vec last)))
    (if (logbitp $lfbits-noname-bit bits)
      (error "Cannot set name of ~s (to ~s)" fn val)
      (setf (svref vec (1- last)) val))))

(defun ccl-closure-function (fn)
  (loop while (logbitp $lfbits-trampoline-bit (ccl-function-bits fn))
    do (setq fn (svref (ccl-function-data fn) 0)))
  fn)

(defun init-ccl-function (fn bslambda bits)
  (setq bits (logandc2 bits (ash 1 $lfbits-noname-bit)))
  (assert (not (ccl-function-native-fn fn)))
  (setf (ccl-function-bslambda fn) bslambda)
  (setf (ccl-function-bits fn) bits)
  (setf (ccl-function-name fn) (cadr bslambda))
  fn)

(defun make-ccl-closure (inner-fn vcells)
  (let ((vec (make-array (+ 1 (length vcells) 1))))
    (setf (svref vec 0) inner-fn)
    (loop for index upfrom 1 for vcell in vcells
      do (setf (svref vec index) vcell)
      finally (setf (svref vec index)
                    (logior (ash 1 $lfbits-noname-bit)
                            (ash 1 $lfbits-trampoline-bit))))
    (%make-ccl-function :subtag subtag-function
                        :data vec
                        :native-fn #'call-closure)))

(defun call-closure (env self args)
  (let* ((vec (ccl-function-data self))
         (last (1- (length vec))))
    (assert (logbitp $lfbits-trampoline-bit (svref vec last)))
    (loop for index from (1- last) above 0 do (push (svref vec index) args))
    (apply-func-in-environment env (svref vec 0) args)))

(defun make-ccl-function (bslambda bits)
  (init-ccl-function (cons-ccl-function) bslambda bits))

(defun ccl-set-macro-function (sym fn)
  (check-type sym ccl-symbol)
  (check-type fn ccl-function)
  (setf (sym-func sym) (make-ccl-uvector :subtag (typekey-subtag :simple-vector)
                                         :data (vector 'macro-apply-code
                                                       fn)))
  fn)

(defun ensure-func (fn-or-sym)
  (let* ((fn (if (typep fn-or-sym 'ccl-symbol) (sym-func fn-or-sym) fn-or-sym)))
    (unless (typep fn 'ccl-function) ;; not macro or special form
      (error "~s is not funcallable" fn-or-sym))
    fn))

(defun ccl-funcall (sym-or-func &rest args)
  (apply-func-in-environment nil (ensure-func sym-or-func) args))

(defun record-debug-info (name doc-info native-type-sym)
  (declare (ignore name doc-info native-type-sym))
  #+NOTYET
  (let* ((arglist (if (listp doc-info) (cddr doc-info)))
         (doc (if (listp doc-info) (car doc-info) doc-info)))
    (record-source-file name native-type-sym)
    (set-documentation name native-type-sym doc)
    (when arglist (record-arglist name arglist))))


;; called for fasloading and also runtime.  Should be pretty similar to the actual
;; %defun, since will keep getting called for fasloaded functions even after bootstrap.
;; Or maybe should replace...
(defun %defun (func doc)
  (check-type func ccl-function)
  (let ((sym (ccl-function-name func)))
    (check-type sym ccl-symbol) ;; no setf functions in level-0
    (record-debug-info sym doc 'function)
    (setf (sym-func sym) func)))
