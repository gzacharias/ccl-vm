(defpackage :ccl-vm (:use :cl))

(in-package :ccl-vm)

(defparameter *env-var-name* 'env)

(defun bseval (form)
  (bseval-in-environment nil form))

(defstruct (vcell (:constructor make-vcell (value)))
  (value () :type ccl-object))

;; Don't ever need the parent, but it's useful for debugging as it gives a full backtrace.
(defstruct bsenv
  (parent nil :type (or bsenv null) :read-only t)
  (self nil :read-only t)
  (locals #() :type (simple-array vcell (*)) :read-only t))

(defmethod print-object ((env bsenv) stream)
  (print-unreadable-object (env stream :type t :identity nil)
    (format stream "~s :parent ~s ~s locals"
            (let ((self (bsenv-self env)))
              (if (consp self) (list (car self) (cadr self)) self))
            (bsenv-parent env)
            (length (bsenv-locals env)))))

(defun bsenv-lvcell (env var-index)
  (aref (bsenv-locals env) var-index))

(defun (setf bsenv-lvcell) (vcell env var-index)
  (setf (aref (bsenv-locals env) var-index) (require-type vcell 'vcell)))

(defun bsenv-lbind (env var-index &optional init)
  (check-type init ccl-object)
  (setf (bsenv-lvcell env var-index) (make-vcell init)))

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

(defun bseval-init-lambda-env (env argspecs values)
  (destructuring-bind (inherited req-lvs opt-lvs rest-lv keys-lv) argspecs
    (cassert (or (not rest-lv) (not keys-lv)))
    (cassert (>= (length values) (length inherited)))
    (loop for lv in inherited for vcell = (pop values)
      do (setf (bsenv-lvcell env lv) vcell))
    (cassert (>= (length values) (length req-lvs)))
    (loop for lv in req-lvs for val = (pop values) do (bsenv-lbind env lv val))
    (loop while (and values opt-lvs)
      for val = (pop values) for (opt-lv nil supp-lv) = (pop opt-lvs)
      do (bsenv-lbind env opt-lv val)
      when supp-lv do (bsenv-lbind env supp-lv t))
    (loop for (opt-lv init supp-lv) in opt-lvs
      do (bsenv-lbind env opt-lv (bseval-in-environment env init))
      when supp-lv do (bsenv-lbind env supp-lv nil))
    (when rest-lv
      (bsenv-lbind env rest-lv (copy-list values)))
    (when keys-lv
      (let* ((allow-other-keys-p (pop keys-lv))
             (not-found-flag keys-lv))
        (loop for (key key-lv init supp-lv) in keys-lv
          do (let ((val (getf values key not-found-flag)))
               (bsenv-lbind env key-lv (if (eq val not-found-flag)
                                         (bseval-in-environment env init)
                                         val))
               (when supp-lv
                 (bsenv-lbind env supp-lv (not (eq val not-found-flag))))))
        (unless allow-other-keys-p
          ;; TODO: check
          )))))

(defun bseval-apply-lambda (env bslambda values)
  (cassert (bseval-op-p bslambda 'bslambda))
  (destructuring-bind (name argspecs body num-vars) (cdr bslambda)
    (declare (ignore name))
    (let ((env (make-bsenv :parent env :self bslambda :locals (make-array num-vars))))
      (bseval-init-lambda-env env argspecs values)
      (bseval-in-environment env body))))

#|
;; V[0] = number of args
;; V[1] = last arg
;; V[nargs] = first arg
(defun init-lexpr-args (values)
  (let* ((nargs (length values))
         (vec (make-array (1+ nargs))))
    (setf (svref vec 0) nargs)
    (loop for val in values as index downfrom nargs
      do (setf (svref vec index) val))
    vec))
|#
(defvar *known-bseval-ops* nil)

(loop while *known-bseval-ops*
  for op = (pop *known-bseval-ops*)
  do (fmakunbound op))

(defmacro defbseval (op-name arglist &body body)
  `(progn
     (pushnew ',op-name *known-bseval-ops*)
     (defmacro ,op-name ,arglist ,@body)))


#+NOTYET (defbseval $bs-lexpr-args (rest-var-index)
  `(init-lexpr-args (bsenv-lvalue ,*env-var-name* ,rest-var-index)))

;;; TODO: WIll this now want an actual function?
#+NOTYET (defbseval $bs-this-function ()
  `(bsenv-self ,*env-var-name*))


(defbseval $bs-lref (index)
  `(bsenv-lvalue ,*env-var-name* ,index))

(defbseval $bs-lset (index value)
  `(setf (bsenv-lvalue ,*env-var-name* ,index) ,value))
  
(defbseval $bs-quote (object)
  (check-type object ccl-object)
  `(quote ,object))


(defbseval $bs-require-fixnum (obj) `(require-type ,obj 'fixnum))
(defbseval $bs-require-gvector (obj) `(require-type ,obj 'gvector))
(defbseval $bs-require-cons (obj) `(require-type ,obj 'cons))
(defbseval $bs-require-list (obj) `(require-type ,obj 'list))
(defbseval $bs-require-symbol (obj) `(require-type ,obj 'ccl-symbol))
(defbseval $bs-require-integer (obj) `(require-type ,obj 'integer))
(defbseval $bs-require-number (obj) `(require-type ,obj 'number))
(defbseval $bs-require-real (obj) `(require-type ,obj 'real))
(defbseval $bs-require-character (obj) `(require-type ,obj 'character))
(defbseval $bs-require-simple-string (obj) `(require-type ,obj 'simple-string))
(defbseval $bs-require-u8 (obj) `(require-type ,obj '(unsigned-byte 8)))

(defbseval $bs-closed-function (func inh)
  (check-type inh list)
  `(make-ccl-closure ,func
                     (list ,@(mapcar (lambda (idx) `(bsenv-lvcell ,*env-var-name* ,idx)) inh))))

#+NOTYET (defbseval $bs-vcell-ref (index)
  `(bsenv-lvcell ,*env-var-name* ,index))

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

#+NOTYET (defbseval $bs-progv (symbols values body) `(progv ,symbols ,values ,body))

(defun bseval-tagbody-forms (form-vector)
  (check-type form-vector simple-vector)
  (loop for form across form-vector for pc upfrom 0
    collect pc ;; may be  used or not, don't care.
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
                  (t form))))

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
    ,@(bseval-tagbody-forms form-vector)))


#+NOTYET (defbseval $bs-go (tag-index pc)
  (check-type tag-index fixnum)
  (check-type pc fixnum)
  #+no `(throw (bsenv-lvalue ,*env-var-name* ,tag-index) ,pc)
  `(error "Hairy tagbody not implemented yet: ~s ~s" ,tag-index ,pc))

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

#+NOTYET (defbseval $bs-apply (fn &rest args)
  (cassert args)
  `(apply-ccl-function ,*env-var-name* ,fn (list* ,@args)))

(defbseval $bs-funcall (fn &rest args)
  `(apply-func-in-environment ,*env-var-name* ,fn (list ,@args)))

;;; *** todo: check native-function
(defvar *trace-funcall* nil)

;;; **TODO: use when-let etc!
           ;;; TODO: need accessor macros for BSLAMBDA'S!

(defun apply-func-in-environment (env fn-or-sym args)
  (when *trace-funcall*
    (format t "~&APPLY ~s to ~s" fn-or-sym args))
  (let ((VALS (MULTIPLE-VALUE-LIST 
  (let* ((fn (ensure-func fn-or-sym))
         (native-fn (ccl-function-native-fn fn))
         (bslambda (ccl-function-bslambda fn)))
    (cond (native-fn
           (if (eq bslambda 'lap)
             (apply native-fn args)
             (funcall native-fn env fn args)))
          ((ccl-function-name fn)
           (destructuring-bind (argspecs body num-vars) (cddr bslambda)
             (FORMAT T "~&COMPILING ~s ~s" (second bslambda) fn)
             (multiple-value-bind (res warnings-p failure-p)
                                  (compile 'bs-func
                                           `(lambda (parent-env self args)
                                              (let ((,*env-var-name* (make-bsenv :parent parent-env :self self :locals (make-array ,num-vars))))
                                                (bseval-init-lambda-env ,*env-var-name* ',argspecs args) ;;; this could be a macro someday
                                                ,body)))
               (declare (ignore warnings-p))
               (when failure-p (error "compilation failed on ~s" fn))
               (setq native-fn (fdefinition res))
               #+CCL (ccl::lfun-name native-fn `(bs-func ,(intern (sym-native-pname (ccl-function-name fn)) *native-package*)))))
           (setf (ccl-function-native-fn fn) native-fn)
           (funcall native-fn env fn args))
          (t ;; else anonymous fn, probably only called once!
           (assert bslambda)
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
;; all this will need to transitiont o call ing the ccl fn
(defmacro def-num-op (bs-op fn-name lisp-op)
  `(progn
     (defbseval ,bs-op (x y) (list ',fn-name x y))
     (defun ,fn-name (x y)
       (if (and (typep x '(or fixnum single-float))
                (typep y '(or fixnum single-float)))
         (let ((res (,lisp-op x y)))
           (if (typep res '(or ccl-fixnum single-float))
             res
             (if (typep res 'bignum)
               (ccl-bignum res)
               (error "~s value ~s not implemented yet" res ',lisp-op))))
         (ccl-funcall (ccl ',lisp-op) x y)))))

           
(def-num-op $bs-add2 ccl-add2 +)
(def-num-op $bs-sub2 ccl-sub2 -)
(def-num-op $bs-mul2 ccl-mul2 *)
(def-num-op $bs-div2 ccl-div2 /)
(def-num-op $bs-ash ccl-ash ash)

(defbseval $bs-lognot (x) `(ccl-lognot ,x))
(defun ccl-lognot (x) (ccl-sub2 -1 x))

(def-num-op $bs-logior2 ccl-logior logior)
(def-num-op $bs-logxor2 ccl-logxor logxor)
(def-num-op $bs-logand2 ccl-logand logand)

(defmacro def-num-test-op (bs-op fn-name lisp-op)
  `(progn
     (defbseval ,bs-op (x y) (list ',fn-name x y))
     (defun ,fn-name (x y)
       (if (and (typep x '(or fixnum single-float))
                (typep y '(or fixnum single-float)))
         (,lisp-op x y)
         (ccl-funcall (ccl ',lisp-op) x y)))))

(def-num-test-op $bs-logbitp ccl-logbitp logbitp)

(def-num-test-op $bs-gt ccl-gt >)
(defbseval $bs-lt (x y) `(ccl-lt ,x ,y))
(defun ccl-lt (x y)
  (if (and (typep x '(or fixnum single-float))
           (typep y '(or fixnum single-float)))
    (< x y)
    ;; Just want to get through this damn file...  #'< not implemented yet when this is called with a bignum/fixnum
    (if (and (ccl-bignum-p x) (fixnump y))
      (let ((v (uvector x)))
        (logbitp 31 (svref v (1- (length v)))))
      (if (and (fixnump x) (ccl-bignum-p y))
        (not (let ((v (uvector y)))
               (logbitp 31 (svref v (1- (length v))))))
        (ccl-funcall (ccl '<) x y)))))


;(def-num-test-op $bs-lt ccl-lt <)
(def-num-test-op $bs-ge ccl-ge >=)
(def-num-test-op $bs-le ccl-le <=)

(defbseval $bs-builtin-lt (x y) `(ccl-lt ,x ,y))
(defbseval $bs-builtin-gt (x y) `(ccl-gt ,x ,y))

(defbseval $bs-builtin-ash (x y) `(ccl-ash ,x ,y))


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

(defbseval $bs-%ilogand2 (x y) `(logand (the ccl-fixnum ,x) (the ccl-fixnum ,y)))

(defbseval $bs-word-to-int (x)
  (let ((word (gensym "WORD")))
    `(let ((,word ,x))
       (if (logbitp 15 ,word)
         (logior ,word ,(ash -1 16))
         (logand ,word ,(lognot (ash -1 16)))))))


(defbseval $bs-%sbchar (str index) `(uvref ,str ,index))

(defbseval $bs-set-%sbchar (str index val) `(setf (uvrev ,str ,index) (require-type ,val 'character)))

(defbseval $bs-%scharcode (str index) `(char-code (uvref ,str ,index)))

(defbseval $bs-set-scharcode (str index val)
  `(setf (svref (uvector ,str) ,index) (code-char ,val)))


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
(defbseval $bs-subtag-misc-set (subtag vec index value) `(typed-uvset ,subtag ,vec ,index ,value))

(defun typed-uvset (subtag vec index value)
  (unless (eq subtag (ccl-uvector-subtag vec))
    (error "Cheating subtag-misc-set not implemented yet for ~s ~s" subtag (ccl-uvector-subtag vec)))
  (uvset vec index value))

(defbseval $bs-subtag-misc-ref (subtag vec index) `(typed-uvref ,subtag ,vec ,index))

(defun typed-uvref (subtag vec index)
  (unless (eq subtag (ccl-uvector-subtag vec))
    (error "Cheating subtag-misc-ref not implemented yet for ~s ~s" subtag (ccl-uvector-subtag vec)))
  (uvref vec index))

(defbseval $bs-uvset (vec index value) `(uvset ,vec ,index ,value))
(defbseval $bs-uvref (vec index) `(uvref ,vec ,index))
(defbseval $bs-uvsize (vec) `(uvsize ,vec))

;; Why not just compile to uvref.   There was a problem, investigate why.
(defbseval $bs-%svref (vec index)
  `(gvref ,vec ,index))

(defbseval $bs-%svset (vec index val)
  `(gvset ,vec ,index ,val))

(defbseval $bs-aref1 (vec index) `(aref1 ,vec ,index))
(defbseval $bs-builtin-aref1 (vec index) `(aref1 ,vec ,index))

(defun aref1 (arr index)
  (if (eql (typecode arr) subtag-vector-header)
    (error "aref1 not implemented for ~s" arr)
    (svref (uvector arr) index)))


(defbseval $bs-aset1 (vec index val) `(aset1 ,vec ,index ,val))
(defbseval $bs-builtin-aset1 (vec index val) `(aset1 ,vec ,index ,val))

(defun aset1 (arr index val)
  (if (eql (typecode arr) subtag-vector-header)
    (error "aset1 not implemented for ~s" arr)
    (setf (svref (uvector arr) index) val)))

;; address is the raw address (aligned so it looks like a fixnum), except in a lexpr, it's a vector
;; can't tell which case at compile time.
#+NOTYET (defbseval $bs-lisp-word-ref (address offset)
  (if (typep address 'fixnum)
    (%lisp-word-ref address offset)
    (svref address offset)))

;; Returns whatever is at address+offset, assumes valid lisp value.
#+NOTYET (defbseval $bs-fixnum-ref (address offset) (%fixnum-ref address offset))

;; returns value as an unsigned int (possibly bignum)
#+NOTYET (defbseval $bs-fixnum-ref-natural (address offset) (%fixnum-ref-natural address offset))



(defbseval $bs-gvector (subtag &rest inits)
  `(gvector ,subtag ,@inits))

(defun gvector (subtag &rest inits)
  (cassert (and (subtag-typekey subtag) (gvector-type-p subtag)))
  (cassert (every #'ccl-object-p inits))
  (make-ccl-uvector :subtag subtag :data (coerce inits 'vector)))


;;; TODO: have compiler break this into make-uvector and init-uvector, don't need all of it.
#+NOTYET (defbseval $bs-uvector (subtag &rest inits)
  (let* ((vector (%alloc-misc (length inits) subtag)))
    (loop for i upfrom 0 for init in inits do (setf (uvref vector i) init))
    vector))

#+NOTYET (defbseval  $bs-init-uvector (vector &rest inits)
  (loop for i upfrom 0 for init in inits do (setf (uvref vector i) init))
  vector)

(defbseval $bs-make-uvector (size subtag)
  `(make-uvector ,size ,subtag))

(defun make-uvector (size subtag &optional (init (cond ((gvector-type-p subtag) nil)
                                                       ((eq subtag subtag-simple-string) #\null)
                                                       (t 0))))
  (let ((conser (or (cdr (assoc subtag *subtag-consers*)) 'make-ccl-uvector)))
    (when (eq conser 'error)
      (error "Cannot make-uvector for type ~s" (subtag-typekey subtag)))
    (funcall conser :subtag subtag
             :data (make-array size :initial-element init))))

;; This has to be split off for level-0.
;;;  TODO: it doesn't need to be split off, subtag is a fixnum so can make decisions at load time if need to.
(defbseval $bs-make-gvector-init (subtag size init)
  (check-type subtag fixnum) ;;so can change order of evaluation
 `(make-uvector ,size ,subtag ,init))
(defbseval $bs-make-ivector-init (subtag size init)
  (check-type subtag fixnum)
 `(make-uvector ,size ,subtag ,init))

;;; The builtin subprims do simple case inline (e.g. fixnum), but punt to lisp function
;;; on more complex cases.
(defbseval $bs-builtin-length (x) `($bs-funcall ($bs-quote ,(ccl 'length)) ,x))

(defbseval $bs-symbol-to-symptr (sym) `(sym-symvector ,sym))
(defbseval $bs-symptr-to-symvector (symptr) symptr)
(defbseval $bs-symvector-to-symptr (symvec) symvec)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbseval $bs-eq (x y) `(eq ,x ,y))
(defbseval $bs-ne (x y) `(not (eq ,x ,y)))
(defbseval $bs-builtin-eql (x y) `(ccl-eql ,x ,y))
(defbseval $bs-not (val) `(not ,val))
(defbseval $bs-yes (val) `(not (null ,val)))

(defbseval $bs-characterp (val) `(characterp ,val))

(defbseval $bs-builtin-seqtype (obj) `(sequence-type ,obj))

(defun sequence-type (x)
  (let* ((typecode (typecode x)))
    (declare (type (unsigned-byte 8) typecode))
    (unless (or (= typecode subtag-vector-header)
                (= typecode subtag-simple-vector)
                (and (ivector-type-p typecode)
                     (>= typecode min-cl-ivector-subtag)))
      (or (listp x)
          (report-bad-arg x 'sequence)))))



(defbseval $bs-lisptag (val) `(lisptag ,val))
(defbseval $bs-fulltag (val) `(fulltag ,val))
(defbseval $bs-typecode (val) `(typecode ,val))

;; TODO: There is a lot of stuff that could be macroexpanded in the ccompiler.  Doing the table
;; jump at eval-time just to open-code it is not worth the complexity.


#+NOTYET (defbseval $bs-gvector-typecode-p (val) (gvector-typecode-p val))
#+NOTYET (defbseval $bs-ivector-typecode-p (val) (ivector-typecode-p val))

(defbseval $bs-istruct-typep (obj type) `(istruct-typep ,obj ,type))

(defun istruct-typep (obj type)
  (and (ccl-uvector-p obj)
       (eql (ccl-uvector-subtag obj) subtag-istruct)
       (eq (car (svref (uvector obj) 0)) type)))

 
;; level 0

(defbseval $bs-current-tcr () 23)

(defbseval $bs-interrupt-level () #+vmthreads *interrupt-level* 0)

(defbseval $bs-with-interrupt-level (level body)
  #+vmthreads `(let ((*interrupt-level* ,level)) ,body)
  (declare (ignore level))
  body)

(defbseval $bs-unbound-marker () `',*unbound-marker*)
(defbseval $bs-slot-unbound-marker () `',*slot-unbound-marker*)
(defbseval $bs-illegal-marker () `',*illegal-marker*)

(defbseval $bs-setf-macptr (ptr value)
  `(setf-macptr ,ptr ,value))

(defun setf-macptr (ptr value)
  (check-type ptr ccl-macptr)
  (check-type value ccl-macptr)
  (setf (svref (uvector ptr) macptr.address-cell)
        (svref (uvector value) macptr.address-cell))
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
                  (consp (cadr entry))
                  (eq (car (cadr entry)) '$bs-quote)
                  (cadr (cadr entry)))))
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

