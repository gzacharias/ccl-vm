(defpackage :ccl-vm (:use :cl))

(in-package :ccl-vm)

(defparameter *env-var-name* 'env)

(defun bseval (form)
  (bseval-in-environment nil form))

(def-uvector-subtype :value-cell (ccl-vcell (:constructor %make-ccl-vcell) (:subtag-conser nil)))

(defun make-vcell (value)
  (%make-ccl-vcell :subtag subtag-value-cell :data (vector value)))

(defmacro vcell-value (vcell)
  `(svref (ccl-vcell-data ,vcell) 0))

(defmethod print-object ((vcell ccl-vcell) stream)
  (let ((value (uvref vcell 0))
        (*print-length* (min (or *print-length* 3) 3))
        (*print-level* (min (or *print-level* 2) 2)))
    (format stream "<VCELL [~s]>" value)))

(def-uvector-subtype :call-frame (bsenv (:constructor %make-bsenv) (:subtag-conser nil))
  (args nil :type list :read-only t) ;; for backtrace
  (parent nil :type (or bsenv null) :read-only t) ;; for backtrace
  (func nil :read-only t))

(defun make-bsenv (parent-env func args num-locals)
  (%make-bsenv :parent parent-env :func func :args args :data (make-array num-locals)))

(defun-inline bsenv-locals (bsenv) (gvector-data bsenv))

(defmethod print-object ((env bsenv) stream)
  (print-unreadable-object (env stream :type t :identity nil)
    (let ((parents (loop for penv = env then (bsenv-parent penv) while penv
                     as self = (bsenv-func penv)
                     collect (if (consp self) (list (car self) (cadr self)) self))))
      (format stream "~s from ~s, ~s locals"
              (car parents)
              (cdr parents)
              (length (bsenv-locals env))))))

(defun bsenv-lvcell (env var-index)
  (require-type (svref (bsenv-locals env) var-index) 'ccl-vcell))

(defun (setf bsenv-lvcell) (vcell env var-index)
  (setf (svref (bsenv-locals env) var-index) (require-type vcell 'ccl-vcell)))

(defmacro bsenv-lbind (env var-index &optional init)
  ;; the init might need to reference the vcell (as in labels)
  ;; so have to bind it first.  *** CHECK IF THIS IS ENOUGH FOR PARALLLEL LABELS.
  (let ((form `(%bsenv-lbind ,env ,var-index)))
    (when init
      (setq form `(setf (vcell-value ,form) ,init)))
    form))

(defun %bsenv-lbind (env var-index)
  (setf (bsenv-lvcell env var-index) (make-vcell nil)))

(defun bsenv-lvalue (env var-index)
  (vcell-value (bsenv-lvcell env var-index)))

(defun (setf bsenv-lvalue) (val env var-index)
  (setf (vcell-value (bsenv-lvcell env var-index)) (require-type val 'ccl-object)))

(defun bseval-op-p  (form op)
  (and (consp form) (eq (car form) op)))

(defun bseval-in-environment (env form)
  (eval `(let ((,*env-var-name* ,env))
           (declare (ignorable ,*env-var-name*))
           ,form)))

(defun bslambda-name (bslambda) (bs-unquote (nth 1 bslambda)))
(defun bslambda-argspecs (bslambda) (nth 2 bslambda))


;; Why not do this at compile time?  Because don't want the compiler using native lisp calls so can be loaded by non-lisp vm's?
(defun bslambda-lambda (bslambda)
  (destructuring-bind (name argspecs body num-vars) (cdr bslambda)
    (declare (ignore name))
    (destructuring-bind (inherited req-lvs opt-lvs rest-lv keys-lvs bits) argspecs
      (declare (ignore bits)) 
      (let ((values-var (make-symbol "VALUES"))
            (args-var (make-symbol "ARGS"))
            (rev-inits nil)
            (special-bindings nil))
        (flet ((bind-form (lv value-form)
                 (if (fixnump lv)
                   `(bsenv-lbind ,*env-var-name* ,lv ,value-form)
                   (let ((old-var (gensym (sym-native-pname lv))))
                     (check-type lv ccl-symbol) ;; not T/NIL, can't bind those.
                     (push (cons lv old-var) special-bindings)
                     `(progn
                        (setq ,old-var (%sym-value ',lv))
                        (%set-sym-value ',lv ,value-form))))))
          (when (or inherited req-lvs)
            (push `(assert (>= (length ,values-var) ,(+ (length inherited) (length req-lvs)))) rev-inits))
          (loop for lv in inherited
            do (check-type lv fixnum)
            do (push `(setf (bsenv-lvcell ,*env-var-name* ,lv) (pop ,values-var)) rev-inits))
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
                do (push (bind-form key-lv `(if (eq (setq ,key-val-var (getf ,values-var ',(bs-unquote key) 'not-found)) 'not-found)
                                              ,init ,key-val-var))
                         key-inits)
                when supp-lv do (push (bind-form supp-lv `(not (eq ,key-val-var 'not-found))) key-inits)
                finally (unless allow-other-keys-p
                          ;; TODO: check
                          ))
              (when key-inits
                (push `(let ,(and key-val-var `(,key-val-var)) ,@(nreverse key-inits)) rev-inits)))))

        (push body rev-inits)
        (setq body `(progn ,@(nreverse rev-inits)))
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
           (let ((,*env-var-name* (make-bsenv parent-env self ,args-var ,num-vars))
                 (,values-var ,args-var))
             (declare (ignorable ,*env-var-name*))
             ,body))))))


(defun bseval-apply-lambda (env bslambda values)
  (cassert (bseval-op-p bslambda 'bslambda))
  (eval `(,(bslambda-lambda bslambda) ',env ',bslambda ',values)))

(defvar *known-bseval-ops* nil)

(loop while *known-bseval-ops*
  for op = (pop *known-bseval-ops*)
  do (fmakunbound op))

(defmacro defbseval (op-name arglist &body body)
  `(progn
     (pushnew ',op-name *known-bseval-ops*)
     (defmacro ,op-name ,arglist ,@body)))


(macrolet ((not-implemented (&rest op-names)
             `(progn
                ,@(mapcar (lambda (op-name)
                            `(defbseval ,op-name (&rest args)
                               (list 'error "~s not implemented yet"
                                     (list 'cons '',op-name (list 'quote args)))))
                          op-names))))
  (not-implemented $bs-debug-trap
                   $bs-complex-realpart
                   $bs-complex-imagpart
                   $bs-make-complex))

(defbseval $bs-lexpr-args (rest-var-index)
  `(init-lexpr-args (bsenv-lvalue ,*env-var-name* ,rest-var-index)))

;; V[0] = number of args
;; V[1] = last arg
;; V[nargs] = first arg
(defun init-lexpr-args (values)
  (make-uvector subtag-lexpr-vector
                (coerce (cons (length values) (nreverse values)) 'vector)))

;; address is the raw address (aligned so it looks like a fixnum), except in a lexpr, it's a vector.
;; Can't tell which case it is at compile time.
(defbseval $bs-lisp-word-ref (address offset)
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

(defbseval $bs-this-function ()
  `(bsenv-func ,*env-var-name*))


(defbseval $bs-lref (index)
  `(bsenv-lvalue ,*env-var-name* ,index))

(defbseval $bs-lset (index value)
  `(setf (bsenv-lvalue ,*env-var-name* ,index) ,value))

(defbseval $bs-quote (object)
  (check-type object ccl-object)
  `(quote ,object))

(defun bs-unquote (form)
  (assert (and (consp form)
               (eq (car form) '$bs-quote)
               (null (cddr form))))
  (cadr form))


(defbseval $bs-require-fixnum (obj) `(require-type ,obj 'ccl-fixnum))
(defbseval $bs-require-gvector (obj) `(require-gvector ,obj))
(defbseval $bs-require-cons (obj) `(require-type ,obj 'cons))
(defbseval $bs-require-list (obj) `(require-type ,obj 'list))
(defbseval $bs-require-symbol (obj) `(require-type ,obj 'ccl-symbol))
(defbseval $bs-require-integer (obj) `(require-type ,obj 'ccl-integer))
(defbseval $bs-require-number (obj) `(require-type ,obj 'ccl-number))
(defbseval $bs-require-real (obj) `(require-type ,obj '(or ccl-integer ccl-ratio)))
(defbseval $bs-require-character (obj) `(require-type ,obj 'character))
(defbseval $bs-require-simple-string (obj) `(require-type ,obj 'ccl-simple-string))
(defbseval $bs-require-simple-vector (obj) `(require-type ,obj 'ccl-simple-vector))
(defbseval $bs-require-u8 (obj) `(require-type ,obj '(unsigned-byte 8)))

(defbseval $bs-closed-function (func inh)
  (check-type inh list)
  `(make-ccl-closure ,func
                     (list ,@(mapcar (lambda (idx) `(bsenv-lvcell ,*env-var-name* ,idx)) inh))))

;; First vcell-ref is ensure-class-metaclass-and-initargs !!!
(defbseval $bs-vcell-ref (index) `(bsenv-lvcell ,*env-var-name* ,index))

(defbseval $bs-block (var-index form)
  (check-type var-index fixnum)
  (let ((tag (gensym "BLOCK-TAG")))
    `(let ((,tag (list "BLOCK-TAG")))
       (bsenv-lbind ,*env-var-name* ,var-index ,tag)
       (catch ,tag
         ,form))))

(defbseval $bs-return-from (tag-index form)
  (check-type tag-index fixnum)
  `(throw (bsenv-lvalue ,*env-var-name* ,tag-index) ,form))

(defbseval $bs-symbol-value (sym) `(sym-value ,sym))

(defbseval $bs-setq-special (sym value) `(setf (sym-value ,sym) ,value))

(defbseval $bs-symbol-function (sym) `(sym-func ,sym))

(defbseval $bs-progn (&rest forms) `(progn ,@forms))
(defbseval $bs-prog1 (valform &rest forms) `(prog1 ,valform ,@forms))
(defbseval $bs-catch (tag form) `(catch ,tag ,form))
(defbseval $bs-throw (target value) `(throw ,target ,value))
(defbseval $bs-unwind-protect (protected cleanup) `(unwind-protect ,protected ,cleanup))
(defbseval $bs-if (test yes no) `(if ,test ,yes ,no))
(defbseval $bs-or (&rest args) `(or ,@args))
(defbseval $bs-values (&rest values) `(values ,@values))
(defbseval $bs-nth-value (n form) `(nth-value ,n ,form))

(defbseval $bs-progv (symbols values body)
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


(defbseval $bs-tagbody (&rest forms)
  `(tagbody ,@(loop for form in forms
                collect (if (eq (car form) '$bs-label)
                          (cadr form)
                          form))))

(defbseval $bs-go (tag) `(go ,tag))

#+OLD (progn
(defun bseval-tagbody-forms (form-vector)
  (check-type form-vector simple-vector)
  (cons 0
        (loop for form across form-vector for pc upfrom 1
          collect (cond ((eq (car form) '$bs-local-go)
                         `(go ,(cadr form)))
                        ((eq (car form) '$bs-local-go-if)
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

(defbseval $bs-local-tagbody (form-vector)
  `(tagbody
    ,@(bseval-tagbody-forms form-vector)))

(defun bseval-tagbody (env tag-lv form-vector)
  (let ((tag (list "TAGBODY-TAG")))
    (bsenv-lbind env tag-lv tag)
    (loop with nforms fixnum = (length form-vector)
      as start = 0 then (catch tag
                          (bseval-tagbody-forms form-vector))
      while (and start (< start nforms)))
    nil))

(defbseval $bs-tagbody (tag-lv form-vector)
  (check-type tag-lv fixnum)
  ;;(error "Hairy tagbody not implemented yet")
  ; We're ok as long as there isn't an actualy GO from somewhere, so err out at $BS-GO.
  ;; (also could easily implement GO to 0, which is the most likely case if it does happen)
  `(tagbody
    (catch 
    ,@(bseval-tagbody-forms form-vector))))

(defbseval $bs-go (tag-index pc)
  (check-type tag-index fixnum)
  (check-type pc fixnum)
  #+no `(throw (bsenv-lvalue ,*env-var-name* ,tag-index) ,pc)
  `(error "Hairy tagbody not implemented yet: ~s ~s" ,tag-index ,pc))
) ;; #+old tagbody



(defbseval $bs-let* (bindings body)
  `(progn
     ,@(loop for (var-index init) in bindings
         do (check-type var-index fixnum)
         collect `(bsenv-lbind ,*ENV-VAR-NAME* ,var-index ,init))
     ,body))

(defbseval $bs-multiple-value-bind (var-indices valform body)
  (let ((vars (loop for var-index in var-indices
                do (check-type var-index fixnum)
                collect (intern (format nil "VAL~d" var-index)))))
    `(progn
       (multiple-value-bind ,vars ,valform
         ,@(loop for var-index in var-indices for var in vars
             collect `(bsenv-lbind ,*env-var-name* ,var-index ,var)))
       ,body)))

(defbseval $bs-mvcall (fn &rest val-forms)
  `(apply-in-environment ,*env-var-name* ,fn
                         (nconc ,@(mapcar (lambda (form) `(multiple-value-list ,form)) val-forms))))

(defbseval $bs-multiple-value-prog1 (val-form other-form)
  `(multiple-value-prog1 ,val-form ,other-form))

(defbseval $bs-apply (fn &rest args)
  (cassert args)
  `(apply-in-environment ,*env-var-name* ,fn (list* ,@args)))

(defbseval $bs-funcall (fn &rest args)
  `(apply-in-environment ,*env-var-name* ,fn (list ,@args)))

(defbseval $bs-multiple-value-list (form) `(multiple-value-list ,form))


(defvar *trace-funcall* nil)

;;; **TODO: use when-let etc!

(defun compile-native-function (ccl-name lambda)
  (multiple-value-bind (res warnings-p failure-p) (compile 'ccl-fn lambda)
    (when failure-p (error "compilation failed on ~s" ccl-name))
    (when warnings-p      ;; All the warnings complained of errors in CCL-FN, give a hit of real name
      (format t "~&in compilation of ~s. ~%" ccl-name))
    (setf res (fdefinition res))
    #+ccl (ccl::lfun-name res `(ccl-fn ,(or (ignore-errors (native ccl-name)) ccl-name)))
    res))
#+hemlock (hemlock::defindent "compile-native-function" 1)

(defun funcall-in-environment (env fn-or-sym &rest args)
  (apply-in-environment env fn-or-sym args))

(defun apply-in-environment (env fn-or-sym args)
  (when *trace-funcall*
    (format t "~&APPLY ~s to ~s" fn-or-sym args))
  (let ((VALS (MULTIPLE-VALUE-LIST 
  (let* ((fn (ensure-func fn-or-sym))
         (native-fn (ccl-function-native-fn fn))
         (bslambda (ccl-function-bslambda fn)))
    (assert (or native-fn (consp bslambda)))
    (cond (native-fn
           (if (eq bslambda 'lap)
             (apply native-fn args)
             (funcall native-fn env fn args)))
          ((ccl-function-name fn)
           (setq native-fn (compile-native-function (ccl-function-name fn) (bslambda-lambda bslambda)))
           (setf (ccl-function-native-fn fn) native-fn)
           (funcall native-fn env fn args))
          (t ;; else anonymous fn, probably only called once!
           (bseval-apply-lambda env bslambda args)))))))
    (when *trace-funcall* (format t "~&RETURNED from ~s: ~s" fn-or-sym vals))
    (apply #'values vals)))

;; TODO: I think we only ever generate this for self-call.  Can just use $bs-funcall and
;; bseval-apply-ccl-function can recognize the lambda case.
#+NOTYET (defbseval $bs-funcall-lambda (bslambda &rest args)
  `(bseval-apply-lambda ,*env-var-name* ,bslambda (list ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  numbers, chars
;;;

(defmacro def-num-op (bs-op fn-name lisp-op ccl-op)
  `(progn
     (defbseval ,bs-op (x y) (list ',fn-name x y))
     (defun ,fn-name (x y)
       (if (and (typep x '(or fixnum single-float))
                (typep y '(or fixnum single-float)))
         (let ((res (,lisp-op x y)))
           (if (typep res '(or ccl-fixnum single-float))
             res
             (ccl-number res)))
         ;; probably need to handle double floats here...
         (ccl-funcall (ccl ',ccl-op) x y)))))

           
(def-num-op $bs-add2 ccl-add2 + +-2)
(def-num-op $bs-sub2 ccl-sub2 - --2)
(def-num-op $bs-mul2 ccl-mul2 * *-2)
(def-num-op $bs-div2 ccl-div2 / /-2)
(def-num-op $bs-ash ccl-ash ash ash)
(def-num-op $bs-logior2 ccl-logior logior logior-2)
(def-num-op $bs-logxor2 ccl-logxor logxor logxor)
(def-num-op $bs-logand2 ccl-logand logand logand-2)

(defbseval $bs-logbitp (x y) `(ccl-logbitp ,x ,y))
(defun ccl-logbitp (x y)
  (if (and (typep x 'fixnum) (typep y 'fixnum))
    (logbitp x y)
    (ccl-funcall (ccl'logbitp) x y)))

;;; *** TODO: do all the builtin fns

(defbseval $bs-lt (x y) `(ccl-lt ,x ,y))

(defun ccl-lt (x y)
  (if (and (typep x '(or fixnum single-float))
           (typep y '(or fixnum single-float)))
    (< x y)
    (if (and (ccl-double-float-p x) (ccl-double-float-p y))
      (< (the double-float (native-double-float x)) (the double-float (native-double-float y)))
      (ccl-funcall (ccl '<-2) x y))))

(defbseval $bs-gt (x y) (let ((_x (gensym)))
                          `(let ((,_x ,x))
                             (ccl-lt ,y ,_x))))


(defbseval $bs-= (x y) `(ccl-= ,x ,y))

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
(defbseval $bs-%i+ (x y) `(fixnumify (+ ,x ,y)))
(defbseval $bs-%i- (x y) `(fixnumify (- ,x ,y)))
(defbseval $bs-iasr (shift x) `(ash ,x (- ,shift)))
(defbseval $bs-ilsr (shift x) `(ash (logand ,full-fixnum-mask ,x) (- ,shift)))
(defbseval $bs-ilsl (shift x) `(fixnumify (ash (logand ,full-fixnum-mask ,x) ,shift)))


(defbseval $bs-word-to-int (x)
  (let ((word (gensym "WORD")))
    `(let ((,word ,x))
       (if (logbitp 15 ,word)
         (logior ,word ,(ash -1 16))
         (logand ,word ,(lognot (ash -1 16)))))))

(defbseval $bs-single-float (num) `(coerce ,num 'single-float))

;; make compiler do this
(defbseval $bs-double-float (num) `(ccl-funcall ,(ccl '%double-float) ,num))


;; Should this do CCL-CHAR-CODE?  are ccl char<>code mappings different?
(defbseval $bs-char-code (char) `(char-code ,char))
(defbseval $bs-code-char (code) `(code-char ,code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequences
;;;

(defbseval $bs-cdr (val) `(cdr ,val))
(defbseval $bs-car (val) `(car ,val))
(defbseval $bs-endp (val) `(endp ,val))
(defbseval $bs-set-car (cons val) `(setf (car ,cons) ,val))
(defbseval $bs-rplaca (cons val) `(rplaca ,cons ,val))
(defbseval $bs-set-cdr (cons val) `(setf (cdr ,cons) ,val))
(defbseval $bs-rplacd (cons val) `(rplacd ,cons ,val))
(defbseval $bs-cons (a b) `(cons ,a ,b))
(defbseval $bs-make-list (size init) `(make-list ,size :initial-element ,init))
(defbseval $bs-list (&rest args) `(list ,@args))
(defbseval $bs-list* (&rest args) `(list* ,@args))

;; In general nx1-1d-vref with declared type will turn into this.
;;  There is at least one case where it's used to cheat, in %init-misc of u8 vec.
(defbseval $bs-subtag-misc-set (subtag vec index value)
  (assert (fixnump subtag))
  (when (eq subtag subtag-bignum)
    ;; Bignums rely on value being truncated on write so get rid of the carry
    (setq value `(logand ,value #xFFFFFFFF)))
  `(typed-uvset ,subtag ,vec ,index ,value))

(defun typed-uvset (subtag vec index value)
  (unless (eq subtag (uvector-subtag vec))
    (error "Cheating subtag-misc-set not implemented yet for ~s ~s" subtag (uvector-subtag vec)))
  (uvset vec index value))

(defbseval $bs-subtag-misc-ref (subtag vec index) `(typed-uvref ,subtag ,vec ,index))

(defun typed-uvref (subtag vec index)
  (unless (eq subtag (uvector-subtag vec))
    (error "Cheating subtag-misc-ref not implemented yet for ~s ~s" subtag (uvector-subtag vec)))
  (uvref vec index))

;;; TODO: go back do distinguishing gvref/set from uvref/set!
(defbseval $bs-uvset (vec index value) `(uvset ,vec ,index ,value))
(defbseval $bs-uvref (vec index) `(uvref ,vec ,index))
(defbseval $bs-uvsize (vec) `(uvsize ,vec))

;; Why not just compile to gvref.   There was a problem, investigate why.
(defbseval $bs-%svref (vec index)
  `(gvref ,vec ,index))

(defbseval $bs-%svset (vec index val)
  `(gvset ,vec ,index ,val))

(defbseval $bs-slot-ref (instance index) `(slot-ref ,instance ,index))

(defbseval $bs-struct-ref (struct index) `(struct-ref ,struct ,index))
(defbseval $bs-struct-set (struct index val) `(struct-set ,struct ,index ,val))

(defbseval $bs-aref1 (vec index) `(aref1 ,vec ,index))

(defun aref1 (arr index)
  (if (eql (typecode arr) subtag-vector-header)
    (ccl-funcall (ccl '%aref1) arr index)
    (gvref arr index)))


(defbseval $bs-aset1 (vec index val) `(aset1 ,vec ,index ,val))

(defun aset1 (arr index val)
  (if (eql (typecode arr) subtag-vector-header)
    (ccl-funcall (ccl '%aset1) arr index val)
    (gvset arr index val)))

;; Returns whatever is at address+offset, assumes valid lisp value.
#+NOTYET (defbseval $bs-fixnum-ref (address offset) (%fixnum-ref address offset))

;; returns value as an unsigned int (possibly bignum)
#+NOTYET (defbseval $bs-fixnum-ref-natural (address offset) (%fixnum-ref-natural address offset))


(defbseval $bs-uvector (subtag &rest inits) `(make-uvector ,subtag (vector ,@inits)))


#+NOTYET (defbseval  $bs-init-uvector (vector &rest inits)
  (loop for i upfrom 0 for init in inits do (setf (uvref vector i) init))
  vector)

(defbseval $bs-make-uvector (size subtag) `(alloc-uvector ,size ,subtag))


;;;  TODO: it doesn't need to be split off, subtag is a fixnum so can make decisions at load time if need to.
(defbseval $bs-make-uvector-init (size subtag init)
 `(alloc-uvector ,size ,subtag ,init))

(defbseval $bs-length (x) `(ccl-funcall (ccl 'length) ,x))

(defbseval $bs-symbol-to-symptr (sym) `(sym-symvector ,sym))
(defbseval $bs-symptr-to-symvector (sym) `(sym-symvector ,sym))
(defbseval $bs-symvector-to-symptr (symvec) `(symvector-sym ,symvec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbseval $bs-eq (x y) `(eq ,x ,y))
(defbseval $bs-ne (x y) `(not (eq ,x ,y)))
(defbseval $bs-eql (x y) `(lap-eql ,x ,y))
(defbseval $bs-not (val) `(not ,val))
(defbseval $bs-yes (val) `(not (null ,val)))

(defbseval $bs-characterp (val) `(characterp ,val))

(defbseval $bs-seqtype (obj) `(listp (require-sequence ,obj)))


(defbseval $bs-lisptag (val) `(lisptag ,val))
(defbseval $bs-fulltag (val) `(fulltag ,val))
(defbseval $bs-typecode (val) `(typecode ,val))

(defbseval $bs-gvector-typecode-p (subtag) `(or (gvector-type-p ,subtag) 0))

(defbseval $bs-ivector-typecode-p (subtag) `(or (ivector-type-p ,subtag) 0))

(defbseval $bs-istruct-typep (obj type) `(istruct-typep ,obj ,type))


;; level 0

(defbseval $bs-current-tcr () 23)

(defbseval $bs-interrupt-level () #+vmthreads *interrupt-level* 0)

(defbseval $bs-with-interrupt-level (level body)
  #+vmthreads `(let ((*interrupt-level* ,level)) ,body)
  (declare (ignore level))
  body)

(defbseval $bs-current-frame-ptr () `(bsenv-parent ,*env-var-name*))


(defbseval $bs-unbound-marker () `',*unbound-marker*)
(defbseval $bs-slot-unbound-marker () `',*slot-unbound-marker*)
(defbseval $bs-illegal-marker () `',*illegal-marker*)

(defbseval $bs-setf-macptr (ptr value)
  `(setf-macptr ,ptr ,value))

(defun setf-macptr (ptr value)
  (check-type ptr ccl-macptr)
  (check-type value ccl-macptr)
  (setf (svref (gvector-data ptr) macptr.address-cell)
        (svref (gvector-data value) macptr.address-cell))
  ptr)


#+NOTYET (defbseval $bs-new-macptr (size clear-p) (bs-%new-gcable-ptr size clear-p))


(defbseval $bs-stack-block (var-index size clear-p body)
  (check-type var-index fixnum)
  (assert (member clear-p '(($bs-quote t) ($bs-quote nil)) :test 'equal))
  (setq clear-p (cadr clear-p))
  `(cffi:with-foreign-pointer (_fptr ,size ,@(when clear-p '(_size)))
     ,@(when clear-p '((clear-mem _fptr _size)))
     (bsenv-lbind ,*ENV-VAR-NAME* ,var-index (make-ccl-macptr _fptr))
     ,body))

;; Big missing cffi feature!
(defun clear-mem (fptr count)
  (loop for offset from 0 below (- count 7) by 8
    do (setf (cffi:mem-ref fptr :uint64 offset) 0)
    finally (loop for offset from offset below count
              do (setf (cffi:mem-ref fptr :uint8 offset) 0))))

(defbseval $bs-inc-macptr (ptr offset) `(make-ccl-macptr (+ (%macptr-value ,ptr) ,offset)))

(defbseval $bs-int-to-macptr (int)
  `(make-ccl-macptr (native-integer ,int)))

#+NOTYET (defbseval $bs-get-macptr (ptr offset) (%get-ptr ptr offset))

(defbseval $bs-macptr-eql (ptr1 ptr2)
  ;; Can just do EQL, once that's debugged
  `(= (%macptr-value ,ptr1) (%macptr-value ,ptr2)))

;;; **TODO: macptr value should be a native pointer! a lot less consing then.

(defun $ff-to-ffi (type)
  (ecase type
    ($ff-signed64 :int64)
    ($ff-unsigned64 :uint64)
    ($ff-signed32 :int32)
    ($ff-unsigned32 :uint32)
    ($ff-signed16 :int16)
    ($ff-unsigned16 :uint16)
    ($ff-unsigned8 :uint8)
    ($ff-signed8 :int8)
    ($ff-address :pointer)
    ($ff-fixnum :int64)
    ($ff-void :void)))

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

(defbseval $bs-macptr-get (ptr byte-offset ff-type)
  (let ((type ($ff-to-ffi ff-type)))
    (ffi-to-ccl type `(cffi:mem-ref (%macptr-ptr ,ptr) ,type ,byte-offset))))


(defbseval $bs-macptr-set (ptr byte-offset ff-type val)
  (let ((type ($ff-to-ffi ff-type)))
    `(setf (cffi:mem-ref (%macptr-ptr ,ptr) ,type ,byte-offset) ,(ccl-to-ffi type val))))
  

#+NOTYET (defbseval $bs-%reference-external-entry-point (arg) (%reference-external-entry-point arg))


;; For now, making the kernel-import fns return ccl values.  Maybe should make them return native
;; and convert..  But in any case, they take ccl values 
(defbseval $bs-kernel-call (name argspecs argvals resultspec)
  (cassert (= (length argspecs) (length argvals)))
  (cassert (string= "KERNEL-IMPORT-" name :end2 (length "KERNEL-IMPORT-")))
  (flet ((typecheck-for (ff-type form)
           `(require-type ,form ',(ecase ff-type
                                    ($ff-address 'ccl-macptr)
                                    (($ff-unsigned64 $ff-signed64) 'ccl-integer)
                                    ($ff-signed32 '(signed-byte 32))
                                    ($ff-unsigned32 '(unsigned-byte 32))
                                    ($ff-signed16 '(signed-byte 16))
                                    ($ff-unsigned16 '(unsigned-byte 16))
                                    ($ff-void 't)))))
    (typecheck-for resultspec
                   `(funcall ',(intern name *native-package*)
                             ,@(loop for argspec in argspecs for argval in argvals
                                 collect (typecheck-for argspec argval))))))

(defbseval $bs-ff-call (entry argspecs argvals resultspec)
  (cassert (= (length argspecs) (length argvals)))
  (let ((sym (and (consp entry)
                  (eq (car entry) '$bs-symbol-value)
                  (bs-unquote (cadr entry)))))
    (check-type sym ccl-symvector))
  (let* ((res-type ($ff-to-ffi resultspec))
         (form `(cffi:foreign-funcall-pointer (cffi:make-pointer ,entry) ()
                                              ,@(loop for argspec in argspecs for val in argvals
                                                  as type = ($ff-to-ffi argspec)
                                                  collect type
                                                  collect (case type
                                                            ((:int64 :uint64) `(native-number ,val))
                                                            (:pointer `(%macptr-ptr ,val))
                                                            (t val)))
                                              ,res-type)))
    (case res-type
      ((:int64 :uint64) `(ccl-number ,form))
      (:pointer `(make-ccl-macptr ,form))
      (t form))))


#+NOTYET (defbseval $bs-debug-trap (arg) (bdbg (list 'debug-trap arg)))

(defconstant $XWRONGTYPE 157)

(defbseval $bs-signalerr (err-no &rest args)
  (if (equal err-no `($bs-quote ,$xwrongtype))
    (destructuring-bind (thing type) args
      `(error "In CCL, value ~s of not of the expected type ~s" ,thing ,type))
    `(error "In CCL, error #~s with args ~s" ',err-no (list ,@args))))

