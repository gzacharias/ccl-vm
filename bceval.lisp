(in-package :ccl-vm)

(defparameter *env-var-name* 'env)

(defun bceval (form)
  (bceval-in-environment nil form))

;;; TODO: maybe let vcell-data be #(), and give it a slot for the value, less space, more direct access
;;;
;;; vcell refs are a bottleneck, need max speed.

(declaim (inline %make-ccl-vcell))
(def-uvector-subtype :value-cell (ccl-vcell (:constructor %make-ccl-vcell (subtag data value)) (:subtag-conser nil))
  (value nil))

(defun-inline make-vcell (value)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (%make-ccl-vcell subtag-value-cell #() value))

(defun-inline vcell-value (vcell)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (ccl-vcell-value (the ccl-vcell vcell)))

(defun-inline set-vcell-value (vcell value)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (setf (ccl-vcell-value (the ccl-vcell vcell)) value))

(defsetf vcell-value set-vcell-value)

(defmethod print-object ((vcell ccl-vcell) stream)
  (let ((value (ccl-vcell-value vcell))
        (*print-length* (min (or *print-length* 3) 3))
        (*print-level* (min (or *print-level* 2) 2)))
    (format stream "<VCELL [~s]>" value)))

(declaim (inline %make-bcenv))
(def-uvector-subtype :call-frame (bcenv (:constructor %make-bcenv (subtag parent func args data)) (:subtag-conser nil))
  (args nil :read-only t)   ;; for backtrace
  (parent nil :read-only t) ;; for backtrace
  (func nil :read-only t))

(defmacro with-bcenv ((num-vars parent-env func args) &rest body)
  `(let* ((_locals (make-array ,num-vars :initial-element 0))
          (,*env-var-name* (%make-bcenv subtag-call-frame ,parent-env ,func ,args _locals)))
     (declare (ignorable ,*env-var-name*)
              ;; This prevents tail calls...
              #+no (dynamic-extent _locals ,*env-var-name*))
     ,@body))

(defun-inline bcenv-locals (bcenv)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (gvector-data bcenv))

(defmethod print-object ((env bcenv) stream)
  (print-unreadable-object (env stream :type t :identity nil)
    (let ((parents (loop for penv = env then (bcenv-parent penv) while penv
                     as self = (bcenv-func penv)
                     collect (if (consp self) (list (car self) (cadr self)) self))))
      (format stream "~s from ~s, ~s locals"
              (car parents)
              (cdr parents)
              (length (bcenv-locals env))))))

(defun-inline bcenv-lvcell (env var-index)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (declare (type bcenv env) (type (unsigned-byte 32) var-index))
  ;; the vcell slot in locals has 0 if the vcell hasn't been allocated
  ;; Don't bother checking for that, will hit an exception trying to read from it.
  (the ccl-vcell ;(or (eql 0) ccl-vcell)
       (svref (bcenv-locals env) var-index)))

(defun-inline set-bcenv-lvcell (env var-index vcell)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (declare (type ccl-vcell vcell) (type bcenv env) (type (unsigned-byte 32) var-index))
  (setf (svref (bcenv-locals env) var-index) vcell))

(defsetf bcenv-lvcell set-bcenv-lvcell)

(defun-inline %bcenv-lbind (env var-index)
  (setf (bcenv-lvcell env var-index) (make-vcell nil)))

(defmacro bcenv-lbind (env var-index &optional init)
  ;; the init might need to reference the vcell (as in labels) so have to
  ;; bind it before eval init.  *** CHECK IF THIS IS ENOUGH FOR PARALLLEL LABELS.
  (let ((form `(%bcenv-lbind (the bcenv ,env) ,var-index)))
    (when init
      (setq form `(setf (vcell-value ,form) ,init)))
    form))

(defun bcenv-lvalue (env var-index)
  (vcell-value (bcenv-lvcell env var-index)))

(defun (setf bcenv-lvalue) (val env var-index)
  (setf (vcell-value (bcenv-lvcell env var-index)) (require-type val 'ccl-object)))

(defun bceval-op-p  (form op)
  (and (consp form) (eq (car form) op)))

(defun bceval-in-environment (env form)
  (eval `(let ((,*env-var-name* ,env))
           (declare (ignorable ,*env-var-name*))
           ,form)))

(defun bclambda-name (bclambda) (bc-unquote (nth 1 bclambda)))
(defun bclambda-argspecs (bclambda) (nth 2 bclambda))

(defun bclambda-lambda (bclambda)
  (assert (eq (car bclambda) 'bclambda))
  (destructuring-bind (name argspecs body num-vars) (cdr bclambda)
    (declare (ignore name))
    (destructuring-bind (inherited req-lvs opt-lvs rest-lv keys-lvs bits) argspecs
      (declare (ignore bits)) 
      (let ((values-var 'values)
            (args-var 'args)
            (rev-inits nil)
            (special-bindings nil))
        (flet ((bind-form (lv value-form)
                 (if (fixnump lv)
                   `(bcenv-lbind ,*env-var-name* ,lv ,value-form)
                   (let* ((var (bc-unquote lv))
                          (old-var (make-symbol (sym-native-pname var))))
                     (push (cons var old-var) special-bindings)
                     `(progn
                        (setq ,old-var (%sym-value ',var))
                        (%set-sym-value ',var ,value-form))))))
          (when (or inherited req-lvs)
            (push `(assert (>= (length ,values-var) ,(+ (length inherited) (length req-lvs)))) rev-inits))
          (loop for lv in inherited
            do (check-type lv fixnum)
            ;; TODO: should typechcek the valeus
            do (push `(setf (bcenv-lvcell ,*env-var-name* ,lv) (require-type (pop ,values-var) 'ccl-vcell))
                     rev-inits))
          (loop for lv in req-lvs do (push (bind-form lv `(pop ,values-var)) rev-inits))
          (loop while opt-lvs
            for (opt-lv init supp-lv) = (pop opt-lvs)
            do (push (bind-form opt-lv `(if ,values-var (car ,values-var) ,init)) rev-inits)
            when supp-lv do (push (bind-form supp-lv `(not (null ,values-var))) rev-inits)
            do (push `(setq ,values-var (cdr ,values-var)) rev-inits))
          (when (and (not rest-lv) (not keys-lvs))
            (push `(assert (null ,values-var)) rev-inits))
          (when rest-lv
            (push (bind-form rest-lv `(copy-list ,values-var)) rev-inits))
          (when keys-lvs
            (let* ((allow-other-keys-p (pop keys-lvs))
                   (key-val-var (and keys-lvs (gensym "KEY-VAL")))
                   (key-inits nil))
              (loop for (key key-lv init supp-lv) in keys-lvs
                do (push (bind-form key-lv `(if (eq (setq ,key-val-var (getf ,values-var ',(bc-unquote key) 'not-found)) 'not-found)
                                              ,init ,key-val-var))
                         key-inits)
                when supp-lv do (push (bind-form supp-lv `(not (eq ,key-val-var 'not-found))) key-inits)
                finally (unless allow-other-keys-p
                          ;; TODO: check
                          ))
              (when key-inits
                (push `(let ,(and key-val-var `(,key-val-var)) ,@(nreverse key-inits)) rev-inits)))))

        (when rev-inits
          (setq body `(let ((,values-var ,args-var))
                        (declare (list ,values-var))
                        ,@(reverse rev-inits)
                        ,body)))
        (when special-bindings
          (setq body
                `(let (,@(loop for sym.var in special-bindings
                           collect `(,(cdr sym.var) 'uninitialized)))
                   (unwind-protect
                       ,body
                     ,@(loop for (sym . var) in special-bindings
                         collect `(unless (eq ,var 'uninitialized)
                                    (%set-sym-value ',sym ,var)))))))

        `(lambda (parent-env self ,args-var)
           (declare (type (or bcenv null) parent-env))
           (with-bcenv (,num-vars parent-env self ,args-var)
             ,body))))))


(defvar *known-bceval-ops* nil)

(loop while *known-bceval-ops*
  for op = (pop *known-bceval-ops*)
  do (fmakunbound op))

(defmacro defbceval (op-name arglist &body body)
  `(progn
     (pushnew ',op-name *known-bceval-ops*)
     (defmacro ,op-name ,arglist ,@body)))


(macrolet ((not-implemented (&rest op-names)
             `(progn
                ,@(mapcar (lambda (op-name)
                            `(defbceval ,op-name (&rest args)
                               (list 'error "~s not implemented yet"
                                     (list 'cons '',op-name (list 'quote args)))))
                          op-names))))
  (not-implemented $bc-debug-trap
                   $bc-complex-realpart
                   $bc-complex-imagpart
                   $bc-make-complex))

(defbceval $bc-lexpr-args (rest-var-index)
  `(init-lexpr-args (bcenv-lvalue ,*env-var-name* ,rest-var-index)))

;; V[0] = number of args
;; V[1] = last arg
;; V[nargs] = first arg
(defun init-lexpr-args (values)
  (make-uvector subtag-lexpr-vector
                (coerce (cons (length values) (nreverse values)) 'vector)))

;; address is the raw address (aligned so it looks like a fixnum), except in a lexpr, it's a vector.
;; Can't tell which case it is at compile time.
(defbceval $bc-lisp-word-ref (address offset)
  (let ((_address (gensym)) (_offset (gensym)))
    `(let ((,_address ,address)
           (,_offset ,offset))
       (if (typep ,_address 'fixnum)
         ;(%lisp-word-ref ,_address ,_offset)
         (error "%LISP-WORD-REF not supported")
         (lexpr-ref ,_address ,_offset)))))

(defun lexpr-ref (vec offset)
  (assert (and (ccl-uvector-p vec) (eql (uvector-subtag vec) subtag-lexpr-vector)))
  (gvref vec offset))

(defbceval $bc-this-function ()
  `(bcenv-func ,*env-var-name*))


(defbceval $bc-lref (index)
  `(bcenv-lvalue ,*env-var-name* ,index))

(defbceval $bc-lset (index value)
  `(setf (bcenv-lvalue ,*env-var-name* ,index) ,value))

(defbceval $bc-quote (object)
  (check-type object ccl-object)
  `(quote ,object))

(defun bc-unquote (form)
  (assert (and (consp form)
               (or (eq (car form) '$bc-quote)
                   (eq (car form) (ccl '$bc-quote)))
               (null (cddr form))))
  (cadr form))


(defbceval $bc-require-fixnum (obj) `(require-type ,obj 'ccl-fixnum))
(defbceval $bc-require-gvector (obj) `(require-gvector ,obj))
(defbceval $bc-require-cons (obj) `(require-type ,obj 'cons))
(defbceval $bc-require-list (obj) `(require-type ,obj 'list))
(defbceval $bc-require-symbol (obj) `(require-type ,obj 'ccl-symbol))
(defbceval $bc-require-integer (obj) `(require-type ,obj 'ccl-integer))
(defbceval $bc-require-number (obj) `(require-type ,obj 'ccl-number))
(defbceval $bc-require-real (obj) `(require-type ,obj '(or ccl-integer ccl-ratio)))
(defbceval $bc-require-character (obj) `(require-type ,obj 'character))
(defbceval $bc-require-simple-string (obj) `(require-type ,obj 'ccl-simple-string))
(defbceval $bc-require-simple-vector (obj) `(require-type ,obj 'ccl-simple-vector))
(defbceval $bc-require-u8 (obj) `(require-type ,obj '(unsigned-byte 8)))

(defbceval $bc-closed-function (func inh)
  (check-type inh list)
  `(make-ccl-closure ,func
                     (list ,@(mapcar (lambda (idx) `(bcenv-lvcell ,*env-var-name* ,idx)) inh))))

(defbceval $bc-vcell-ref (index) `(bcenv-lvcell ,*env-var-name* ,index))

(defbceval $bc-block (var-index form)
  (check-type var-index fixnum)
  (let ((tag (gensym "BLOCK-TAG")))
    `(let ((,tag (list "BLOCK-TAG")))
       (bcenv-lbind ,*env-var-name* ,var-index ,tag)
       (catch ,tag
         ,form))))

(defbceval $bc-return-from (tag-index form)
  (check-type tag-index fixnum)
  `(throw (bcenv-lvalue ,*env-var-name* ,tag-index) ,form))

(defbceval $bc-symbol-value (sym) `(sym-value ,sym))

(defbceval $bc-setq-special (sym value) `(setf (sym-value ,sym) ,value))

(defbceval $bc-symbol-function (sym) `(sym-func ,sym))

(defbceval $bc-progn (&rest forms) `(progn ,@forms))
(defbceval $bc-prog1 (valform &rest forms) `(prog1 ,valform ,@forms))
(defbceval $bc-catch (tag form) `(catch ,tag ,form))
(defbceval $bc-throw (target value) `(throw ,target ,value))
(defbceval $bc-unwind-protect (protected cleanup) `(unwind-protect ,protected ,cleanup))
(defbceval $bc-if (test yes no) `(if ,test ,yes ,no))
(defbceval $bc-or (&rest args) `(or ,@args))
(defbceval $bc-values (&rest values) `(values ,@values))
(defbceval $bc-nth-value (n form) `(nth-value ,n ,form))

(defbceval $bc-progv (symbols values body)
  (let ((old-vals (gensym "OLD-VALS"))
        (syms (gensym "SYMS")))
    `(let* ((,syms ,symbols)
            (,old-vals (dbind-save ,syms)))
       ;#+vm-threads (mapcar #'ensure-binding-index syms)
       (unwind-protect (progn
                         (dbind-bind ,syms ,values)
                         ,body)
         (when ,old-vals (dbind-restore ,syms ,old-vals))))))

(defun dbind-save (symbols)
  (loop for sym in symbols
    as symv = (sym-symvector sym)
    as bits = (gvref symv sym.bits)
    do (when (or (logbitp $sym_vbit_global bits)
                 (logbitp $sym_vbit_constant bits))
         (error "Cannot bind global value ~s" sym))
    collect (%sym-value symv)))

(defun dbind-bind (symbols values)
  (assert (eq (length symbols) (length values)))
  (loop for sym in symbols for val in values
    do (setf (sym-value sym) val)))

(defun dbind-restore (symbols old-values)
  (loop for sym in symbols for val in old-values
    do (%set-sym-value sym val)))


(defbceval $bc-tagbody (&rest forms)
  `(tagbody ,@(loop for form in forms
                collect (if (eq (car form) '$bc-label)
                          (cadr form)
                          form))))

(defbceval $bc-go (tag) `(go ,tag))

#+OLD (progn
(defun bceval-tagbody-forms (form-vector)
  (check-type form-vector simple-vector)
  (cons 0
        (loop for form across form-vector for pc upfrom 1
          collect (cond ((eq (car form) '$bc-local-go)
                         `(go ,(cadr form)))
                        ((eq (car form) '$bc-local-go-if)
                         (destructuring-bind (test yes-form yes-target no-form no-target) (cdr form)
                           `(if ,test
                              (progn
                                ,yes-form ;; could be nil
                                ,@(and yes-target `((go ,yes-target))))
                              (progn
                                ,no-form ;; could be nil
                                ,@(and no-target `((go ,no-target)))))))
                        (t form))
          collect pc))) ;; may be  used or not, don't care.

(defbceval $bc-local-tagbody (form-vector)
  `(tagbody
    ,@(bceval-tagbody-forms form-vector)))

(defun bceval-tagbody (env tag-lv form-vector)
  (let ((tag (list "TAGBODY-TAG")))
    (bcenv-lbind env tag-lv tag)
    (loop with nforms fixnum = (length form-vector)
      as start = 0 then (catch tag
                          (bceval-tagbody-forms form-vector))
      while (and start (< start nforms)))
    nil))

(defbceval $bc-tagbody (tag-lv form-vector)
  (check-type tag-lv fixnum)
  ;;(error "Hairy tagbody not implemented yet")
  ; We're ok as long as there isn't an actualy GO from somewhere, so err out at $bc-GO.
  ;; (also could easily implement GO to 0, which is the most likely case if it does happen)
  `(tagbody
    (catch 
    ,@(bceval-tagbody-forms form-vector))))

(defbceval $bc-go (tag-index pc)
  (check-type tag-index fixnum)
  (check-type pc fixnum)
  #+no `(throw (bcenv-lvalue ,*env-var-name* ,tag-index) ,pc)
  `(error "Hairy tagbody not implemented yet: ~s ~s" ,tag-index ,pc))
) ;; #+old tagbody



(defbceval $bc-let* (bindings body)
  `(progn
     ,@(loop for (var-index init) in bindings
         do (check-type var-index fixnum)
         collect `(bcenv-lbind ,*env-var-name* ,var-index ,init))
     ,body))

(defbceval $bc-multiple-value-bind (var-indices valform body)
  (let ((vars (loop for var-index in var-indices
                do (check-type var-index fixnum)
                collect (intern (format nil "VAL~d" var-index)))))
    `(progn
       (multiple-value-bind ,vars ,valform
         ,@(loop for var-index in var-indices for var in vars
             collect `(bcenv-lbind ,*env-var-name* ,var-index ,var)))
       ,body)))

(defbceval $bc-mvcall (fn &rest val-forms)
  `(apply-in-environment ,*env-var-name* ,fn
                         (nconc ,@(mapcar (lambda (form) `(multiple-value-list ,form)) val-forms))))

(defbceval $bc-multiple-value-prog1 (val-form other-form)
  `(multiple-value-prog1 ,val-form ,other-form))

(defbceval $bc-apply (fn &rest args)
  (cassert args)
  `(apply-in-environment ,*env-var-name* ,fn (list* ,@args)))

(defbceval $bc-funcall (fn &rest args)
  `(apply-in-environment ,*env-var-name* ,fn (list ,@args)))

(defbceval $bc-multiple-value-list (form) `(multiple-value-list ,form))


(defvar *trace-funcall* nil)

(defvar *verbose-auto-compile* nil)

;; There are 1349 functions that are compiled while loading, i.e. they're loaded, and then they're used while loading other files.
;; Is there any value in compiling them sooner?
(defun compile-native-function (ccl-name lambda)
  (when (ccl-instance-p ccl-name)
    (setq ccl-name (instance-slot ccl-name %method.name)))
  (let* ((native-name (ignore-errors (native ccl-name))) ;; don't know how to nativize symbols in non-std pkgs (e.g. arch::)
         (fn-name (if (consp native-name)
                    `(ccl-fn ,@native-name)
                    `(ccl-fn ,(or native-name ccl-name)))))
    (when *verbose-auto-compile*
      (format t "~&Compiling ~s" (or native-name ccl-name)))
    (multiple-value-bind (res warnings-p failure-p) (compile 'ccl-fn lambda)
      (when failure-p (error "compilation failed on ~s" ccl-name))
      (when warnings-p      ;; All the warnings complained of errors in CCL-FN, give a hint of real name
        (format t "~&in compilation of ~s. ~%" ccl-name))
      (setf res (fdefinition res))
      #+ccl (ccl::lfun-name res fn-name)
      #+sbcl (setf (sb-kernel:%fun-name res) fn-name)
      res)))

(defun funcall-in-environment (env fn-or-sym &rest args)
  (apply-in-environment env fn-or-sym args))

(defun-inline do-apply-in-environment (env fn-or-sym args)
  (declare (optimize (speed 3) (safety 0) (space 0))) ;; this is a bottleneck fn.
  (let* ((fn (ensure-func fn-or-sym))
         (native-fn (ccl-function-native-fn fn))
         (bclambda (ccl-function-bclambda fn)))
    (declare (type ccl-function fn))
    (cond (native-fn
           (if (eq bclambda 'lap)
             (apply (the function native-fn) args)
             (funcall (the function native-fn) env fn args)))
          ;; This comes pretty close to what we want.  When loading, there are 3 fns called total of 1273 times
          ;; that don't get compiled, I think they are from instance :initform's.  There are about 114 fns that are
          ;; only called once that get compiled because have lfun bits.
          ((or (ccl-function-name fn) (not (eql 0 (ccl-function-bits fn))))
           (setq native-fn (compile-native-function (ccl-function-name fn) (bclambda-lambda bclambda)))
           (setf (ccl-function-native-fn fn) native-fn)
           (funcall native-fn env fn args))
          (t ;; else anonymous fn of no args, probably only called once, don't bother compiling.
           ;; Note: IN CCL, this will get compiled anyway because it's a lambda application!
           (eval `(,(bclambda-lambda bclambda) ',env ',fn ',args))))))


(defun apply-in-environment (env fn-or-sym args)
  (if (not *trace-funcall*)
    (do-apply-in-environment env fn-or-sym args)
    (locally
      (declare (notinline do-apply-in-environment))
      (format t "~&APPLY ~s to ~s" fn-or-sym args)
      (let ((vals (multiple-value-list (do-apply-in-environment env fn-or-sym args))))
        (format t "~&RETURNED from ~s: ~s" fn-or-sym vals)
        (apply #'values vals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  numbers, chars
;;;

(defmacro def-int-op (bc-op fn-name lisp-op ccl-op)
  `(progn
     (defbceval ,bc-op (x y) (list ',fn-name x y))
     (defun ,fn-name (x y)
       (if (and (typep x 'fixnum)
                (typep y 'fixnum))
         (let ((res (,lisp-op x y)))
           (if (typep res 'ccl-fixnum)
             res
             (ccl-bignum res)))
         (ccl-funcall (ccl ',ccl-op) x y)))))

(def-int-op $bc-ash ccl-ash ash ash)
(def-int-op $bc-logior2 ccl-logior logior logior-2)
(def-int-op $bc-logxor2 ccl-logxor logxor logxor)
(def-int-op $bc-logand2 ccl-logand logand logand-2)

(defmacro def-num-op (bc-op fn-name lisp-op ccl-op)
  `(progn
     (defbceval ,bc-op (x y) (list ',fn-name x y))
     (defun ,fn-name (x y)
       (if (and (typep x '(or fixnum single-float))
                (typep y '(or fixnum single-float)))
         (let ((res (,lisp-op x y)))
           (if (typep res '(or ccl-fixnum single-float))
             res
             (ccl-number res)))
         (if (and (ccl-double-float-p x) (ccl-double-float-p y))
           (ccl-number (,lisp-op (the double-float (native-double-float x)) (the double-float (native-double-float y))))
           (ccl-funcall (ccl ',ccl-op) x y))))))

(def-num-op $bc-add2 ccl-add2 + +-2)
(def-num-op $bc-sub2 ccl-sub2 - --2)
(def-num-op $bc-mul2 ccl-mul2 * *-2)
(def-num-op $bc-div2 ccl-div2 / /-2)

(defbceval $bc-logbitp (x y) `(ccl-logbitp ,x ,y))
(defun ccl-logbitp (x y)
  (if (and (typep x 'fixnum) (typep y 'fixnum))
    (logbitp x y)
    (ccl-funcall (ccl'logbitp) x y)))

;;; *** TODO: do all the builtin fns

(defbceval $bc-lt (x y) `(ccl-lt ,x ,y))

(defun ccl-lt (x y)
  (if (and (typep x '(or fixnum single-float))
           (typep y '(or fixnum single-float)))
    (< x y)
    (if (and (ccl-double-float-p x) (ccl-double-float-p y))
      (< (the double-float (native-double-float x)) (the double-float (native-double-float y)))
      (ccl-funcall (ccl '<-2) x y))))

(defbceval $bc-gt (x y) (let ((_x (gensym)))
                          `(let ((,_x ,x))
                             (ccl-lt ,y ,_x))))


(defbceval $bc-= (x y) `(ccl-= ,x ,y))

(defun ccl-= (x y)
  (if (and (typep x '(or fixnum single-float))
           (typep y '(or fixnum single-float)))
    (= x y)
    (if (and (ccl-double-float-p x) (ccl-double-float-p y))
      ;(uvector-equal x y) ;; ok assume they're normalized?
      (= (the double-float (native-double-float x)) (the double-float (native-double-float y)))
      (ccl-funcall (ccl '=-2) x y))))

(declaim (ftype (function (t) ccl-fixnum) fixnumify))

(defun fixnumify (res)
  (cond ((typep res 'ccl-fixnum)
         res)
        (t
         (check-type res integer)
         (if (logbitp (1- num-fixnum-bits) res)
           (logior res (ash -1 (1- num-fixnum-bits)))
           (logandc2 res (ash -1 (1- num-fixnum-bits)))))))
    
;; Turns out there is code in l0-hash that depends on at least %i+  doing moodular +
(defbceval $bc-%i+ (x y) `(fixnumify (+ ,x ,y)))
(defbceval $bc-%i- (x y) `(fixnumify (- ,x ,y)))
(defbceval $bc-iasr (shift x) `(ash ,x (- ,shift)))
(defbceval $bc-ilsr (shift x) `(ash (logand ,full-fixnum-mask ,x) (- ,shift)))
(defbceval $bc-ilsl (shift x) `(fixnumify (ash (logand ,full-fixnum-mask ,x) ,shift)))


(defbceval $bc-word-to-int (x)
  (let ((word (gensym "WORD")))
    `(let ((,word ,x))
       (if (logbitp 15 ,word)
         (logior ,word ,(ash -1 16))
         (logand ,word ,(lognot (ash -1 16)))))))

(defbceval $bc-single-float (num) `(coerce (native ,num) 'single-float))

;; make compiler do this
(defbceval $bc-double-float (num) `(ccl-funcall ,(ccl '%double-float) ,num))


(defbceval $bc-setf-double-float (result num)
  (let ((res (gensym)))
    `(let ((,res ,result)) (lap-%copy-double-float ,num ,res))))


;; Should this do CCL-CHAR-CODE?  are ccl char<>code mappings different?
(defbceval $bc-char-code (char) `(char-code ,char))
(defbceval $bc-code-char (code) `(code-char ,code))
(defbceval $bc-base-char-p (char) `(typep ,char 'base-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequences
;;;

(defbceval $bc-cdr (val) `(cdr ,val))
(defbceval $bc-car (val) `(car ,val))
(defbceval $bc-endp (val) `(endp ,val))
(defbceval $bc-set-car (cons val) `(setf (car ,cons) ,val))
(defbceval $bc-rplaca (cons val) `(rplaca ,cons ,val))
(defbceval $bc-set-cdr (cons val) `(setf (cdr ,cons) ,val))
(defbceval $bc-rplacd (cons val) `(rplacd ,cons ,val))
(defbceval $bc-cons (a b) `(cons ,a ,b))
(defbceval $bc-make-list (size init) `(make-list ,size :initial-element ,init))
(defbceval $bc-list (&rest args) `(list ,@args))
(defbceval $bc-list* (&rest args) `(list* ,@args))

;; In general nx1-1d-vref with declared type will turn into this.
;;  There is at least one case where it's used to cheat, in %init-misc of u8 vec.
(defbceval $bc-subtag-misc-set (subtag vec index value)
  (assert (fixnump subtag))
  (when (eq subtag subtag-bignum)
    ;; Bignums rely on value being truncated on write so get rid of the carry
    (setq value `(logand ,value #xFFFFFFFF)))
  `(typed-uvset ,subtag ,vec ,index ,value))

(defun typed-uvset (subtag vec index value)
  (unless (eq subtag (uvector-subtag vec))
    (error "Cheating subtag-misc-set not implemented yet for ~s ~s" subtag (uvector-subtag vec)))
  (uvset vec index value))

(defbceval $bc-subtag-misc-ref (subtag vec index) `(typed-uvref ,subtag ,vec ,index))

(defun typed-uvref (subtag vec index)
  (unless (eq subtag (uvector-subtag vec))
    (error "Cheating subtag-misc-ref not implemented yet for ~s ~s" subtag (uvector-subtag vec)))
  (uvref vec index))

;;; TODO: go back do distinguishing gvref/set from uvref/set!
(defbceval $bc-uvset (vec index value) `(uvset ,vec ,index ,value))
(defbceval $bc-uvref (vec index) `(uvref ,vec ,index))
(defbceval $bc-uvsize (vec) `(uvsize ,vec))

;; Why not just compile to gvref.   There was a problem, investigate why.
(defbceval $bc-%svref (vec index)
  `(gvref ,vec ,index))

(defbceval $bc-%svset (vec index val)
  `(gvset ,vec ,index ,val))

(defbceval $bc-slot-ref (instance index) `(slot-ref ,instance ,index))

(defbceval $bc-struct-ref (struct index) `(struct-ref ,struct ,index))
(defbceval $bc-struct-set (struct index val) `(struct-set ,struct ,index ,val))

(defbceval $bc-aref1 (vec index) `(aref1 ,vec ,index))

(defun aref1 (arr index)
  (if (eql (ccl-typecode arr) subtag-vector-header)
    (ccl-funcall (ccl '%aref1) arr index)
    (gvref arr index)))


(defbceval $bc-aset1 (vec index val) `(aset1 ,vec ,index ,val))

(defun aset1 (arr index val)
  (if (eql (ccl-typecode arr) subtag-vector-header)
    (ccl-funcall (ccl '%aset1) arr index val)
    (gvset arr index val)))

;; Returns whatever is at address+offset, assumes valid lisp value.
#+NOTYET (defbceval $bc-fixnum-ref (address offset) (%fixnum-ref address offset))

;; returns value as an unsigned int (possibly bignum)
#+NOTYET (defbceval $bc-fixnum-ref-natural (address offset) (%fixnum-ref-natural address offset))


(defbceval $bc-uvector (subtag &rest inits) `(make-uvector ,subtag (vector ,@inits)))


#+NOTYET (defbceval  $bc-init-uvector (vector &rest inits)
  (loop for i upfrom 0 for init in inits do (setf (uvref vector i) init))
  vector)

(defbceval $bc-make-uvector (size subtag) `(alloc-uvector ,size ,subtag))


;;;  TODO: it doesn't need to be split off, subtag is a fixnum so can make decisions at load time if need to.
(defbceval $bc-make-uvector-init (size subtag init)
 `(alloc-uvector ,size ,subtag ,init))

(defbceval $bc-length (x) `(ccl-funcall (ccl 'length) ,x))

(defbceval $bc-symbol-to-symptr (sym) `(sym-symvector ,sym))
(defbceval $bc-symptr-to-symvector (sym) `(sym-symvector ,sym))
(defbceval $bc-symvector-to-symptr (symvec) `(symvector-sym ,symvec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbceval $bc-eq (x y) `(eq ,x ,y))
(defbceval $bc-ne (x y) `(not (eq ,x ,y)))
(defbceval $bc-eql (x y) `(lap-eql ,x ,y))
(defbceval $bc-not (val) `(not ,val))
(defbceval $bc-yes (val) `(not (null ,val)))

(defbceval $bc-characterp (val) `(characterp ,val))

(defbceval $bc-seqtype (obj) `(listp (require-sequence ,obj)))


(defbceval $bc-lisptag (val) `(lisptag ,val))
(defbceval $bc-fulltag (val) `(fulltag ,val))
(defbceval $bc-typecode (val) `(ccl-typecode ,val))

(defbceval $bc-gvector-typecode-p (subtag) `(or (gvector-type-p ,subtag) 0))

(defbceval $bc-ivector-typecode-p (subtag) `(or (ivector-type-p ,subtag) 0))

(defbceval $bc-istruct-typep (obj type) `(istruct-typep ,obj ,type))


;; level 0

(defbceval $bc-current-tcr () 23)

(defbceval $bc-interrupt-level () #+vmthreads *interrupt-level* 0)

(defbceval $bc-with-interrupt-level (level body)
  #+vmthreads `(let ((*interrupt-level* ,level)) ,body)
  (declare (ignore level))
  body)

(defbceval $bc-current-frame-ptr () `(bcenv-parent ,*env-var-name*))


(defbceval $bc-unbound-marker () `',*unbound-marker*)
(defbceval $bc-slot-unbound-marker () `',*slot-unbound-marker*)
(defbceval $bc-illegal-marker () `',*illegal-marker*)

(defbceval $bc-setf-macptr (ptr value)
  `(setf-macptr ,ptr ,value))

(defun setf-macptr (ptr value)
  (check-type ptr ccl-macptr)
  (check-type value ccl-macptr)
  (setf (svref (gvector-data ptr) macptr.address)
        (svref (gvector-data value) macptr.address))
  ptr)


#+NOTYET (defbceval $bc-new-macptr (size clear-p) (bc-%new-gcable-ptr size clear-p))


(defbceval $bc-stack-block (var-index size clear-p body)
  (check-type var-index fixnum)
  (assert (member clear-p '(($bc-quote t) ($bc-quote nil)) :test 'equal))
  (setq clear-p (cadr clear-p))
  `(cffi:with-foreign-pointer (_fptr ,size ,@(when clear-p '(_size)))
     ,@(when clear-p '((clear-mem _fptr _size)))
     (bcenv-lbind ,*ENV-VAR-NAME* ,var-index (make-ccl-macptr _fptr))
     ,body))

;; Big missing cffi feature!
(defun clear-mem (fptr count)
  (loop for offset from 0 below (- count 7) by 8
    do (setf (cffi:mem-ref fptr :uint64 offset) 0)
    finally (loop for offset from offset below count
              do (setf (cffi:mem-ref fptr :uint8 offset) 0))))

(defbceval $bc-inc-macptr (ptr offset) `(make-ccl-macptr (+ (%macptr-value ,ptr) ,offset)))

(defbceval $bc-int-to-macptr (int) `(make-ccl-macptr (native-integer ,int)))

(defbceval $bc-macptr-to-int (macptr) `(%macptr-value ,macptr))

#+NOTYET (defbceval $bc-get-macptr (ptr offset) (%get-ptr ptr offset))

(defbceval $bc-macptr-eql (ptr1 ptr2)
  ;; Can just do EQL, once that's debugged
  `(= (%macptr-value ,ptr1) (%macptr-value ,ptr2)))

;;; **TODO: macptr value should be a pointer rather than integer, a lot less consing then.


;; must match FF-xxx constants in cvm2.lisp
(defparameter *ff-types* #(:int64 :int32 :int16 :int8 :uint64 :uint32 :uint16 :uint8 :float :double :pointer :void))
(defun ff-type (index)
  (if (< index 256)
    index
    (svref *ff-types* (- index 256))))

(defun ffi-to-ccl (type form)
  (case type
    ((:int64 :uint64) `(ccl-number ,form))
    (:pointer `(make-ccl-macptr ,form))
    (t form)))

(defun ccl-to-ffi (type form)
  (case type
    ((:int64 :uint64) `(native-integer ,form))
    (:pointer `(%macptr-ptr ,form))
    (t form)))

(defbceval $bc-macptr-get (ptr byte-offset ff-type)
  (setq ff-type (ff-type ff-type))
  (ffi-to-ccl ff-type `(cffi:mem-ref (%macptr-ptr ,ptr) ,ff-type ,byte-offset)))


(defbceval $bc-macptr-set (ptr byte-offset ff-type val)
  (setq ff-type (ff-type ff-type))
  `(setf (cffi:mem-ref (%macptr-ptr ,ptr) ,ff-type ,byte-offset) ,(ccl-to-ffi ff-type val)))
  

#+NOTYET (defbceval $bc-%reference-external-entry-point (arg) (%reference-external-entry-point arg))


;; For now, making the kernel-import fns return ccl values.  Maybe should make them return native
;; and convert..  But in any case, they take ccl values 
(defbceval $bc-kernel-call (name argspecs argvals resultspec)
  (cassert (= (length argspecs) (length argvals)))
  (cassert (string= "KERNEL-IMPORT-" name :end2 (length "KERNEL-IMPORT-")))
  (flet ((typecheck-for (ff-type form)
           `(require-type ,form ',(ecase (ff-type ff-type)
                                    (:pointer 'ccl-macptr)
                                    ((:uint64 :int64) 'ccl-integer)
                                    (:int32 '(signed-byte 32))
                                    (:uint32 '(unsigned-byte 32))
                                    (:int16 '(signed-byte 16))
                                    (:uint16 '(unsigned-byte 16))
                                    (:void 't)))))
    (typecheck-for resultspec
                   `(funcall ',(intern name *native-package*)
                             ,@(loop for argspec in argspecs for argval in argvals
                                 collect (typecheck-for argspec argval))))))

(defbceval $bc-ff-call (entry argspecs argvals resultspec)
  (cassert (= (length argspecs) (length argvals)))
  (assert (and (consp entry)
               (or (and (eq (car entry) '$bc-symbol-value)
                        (ccl-symvector-p (bc-unquote (cadr entry))))
                   (and (eq (car entry) '$bc-%reference-external-entry-point)
                        (istruct-typep (bc-unquote (cadr entry)) (ccl'external-entry-point))))))
  (let* ((form `(cffi:foreign-funcall-pointer (cffi:make-pointer ,entry) ()
                                              ,@(loop for ff-type in argspecs for val in argvals
                                                  collect (setq ff-type (ff-type ff-type))
                                                  collect (case ff-type
                                                            ((:int64 :uint64) `(native-number ,val))
                                                            (:pointer `(%macptr-ptr ,val))
                                                            (t val)))
                                              ,(setq resultspec (ff-type resultspec)))))
    (case resultspec
      ((:int64 :uint64) `(ccl-number ,form))
      (:pointer `(make-ccl-macptr ,form))
      (t form))))


#+NOTYET (defbceval $bc-debug-trap (arg) (bdbg (list 'debug-trap arg)))

(defconstant $XWRONGTYPE 157)

(defbceval $bc-signalerr (err-no &rest args)
  (if (equal err-no `($bc-quote ,$xwrongtype))
    (destructuring-bind (thing type) args
      `(error "In CCL, value ~s of not of the expected type ~s" ,thing ,type))
    `(error "In CCL, error #~s with args ~s" ',err-no (list ,@args))))

