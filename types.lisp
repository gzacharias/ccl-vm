(in-package :ccl-vm)

(deftype ccl-fixnum () `(signed-byte ,num-fixnum-bits))

#|  NAMING:

(1)  CCL-UVEC  - CCL-GVEC/CCL-IVEC
(2) SYMVEC, SYM
(3) PKG
(4) 

|#

;; a simple vector for everything, until there's a good reason not to.
(defstruct (ccl-uvector (:constructor %raw-make-uvector) (:conc-name uvector-))
  (subtag 0 :type (unsigned-byte 8) :read-only t)
  (data #() :type (or simple-vector cffi:foreign-pointer)))

;; Perhaps should give this a field in the header...
(defun heap-vector-p (uvec)
  (and (ccl-uvector-p uvec)
       (typep (uvector-data uvec) 'cffi:foreign-pointer)))

(defmacro with-uvector-data ((var obj) heap-vector-body &body body)
  `(let ((,var (uvector-data ,obj)))
     (if (typep ,var 'cffi:foreign-pointer)
       ,(if (eq heap-vector-body :error)
          '(error "Heap vectors not supported here")
          heap-vector-body)
       (progn ,@body))))

(defconstant arrayh.rank 0)
(defconstant arrayh.physsize 1)
(defconstant arrayh.data-vector 2)
(defconstant arrayh.displacement 3)
(defconstant arrayh.flags 4)
(defconstant arrayh.first-dimension 5)

(defconstant array.flags-subtag-byte (byte 8 8))

(defconstant vectorh.logsize 0) ;; fill pointer or physsize
(defconstant vectorh.physsize arrayh.physsize)
(defconstant vectorh.data-vector arrayh.data-vector)
(defconstant vectorh.displacement arrayh.displacement)
(defconstant vectorh.flags arrayh.flags)



(defparameter *subtag-consers* ())

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

(defun make-uvector (subtag data)
  (let ((conser (or (cdr (assoc subtag *subtag-consers*)) '%raw-make-uvector)))
    (when (eq conser 'error)
      (error "Cannot make-uvector for type ~s" (subtag-typekey subtag)))
    (funcall conser :subtag subtag :data data)))

(defun alloc-uvector (size subtag &optional (init (cond ;;((gvector-type-p subtag) nil) no, CCL inits all arrays to 0!
                                                   ((eq subtag subtag-simple-string) #\null)
                                                   (t 0))))
  (make-uvector subtag (make-array size :initial-element init)))

(defmethod print-object ((obj ccl-uvector) stream)
  (let ((typekey (subtag-typekey (uvector-subtag obj))))
    (let ((text (uvector-print-text typekey obj)))
      (if text
        (format stream "{~a}" text)
        (format stream "<~s ~s~a ~s elts>"
                (type-of obj)
                typekey
                (with-uvector-data (data obj) "(Heap Vec)"  "")
                (uvsize obj))))))

(defmethod uvector-print-text ((type t) obj)
  (declare (ignore obj))
  nil)

(defmacro def-uvector-print-text (typekey fn args &body body)
  `(progn
     (defmethod uvector-print-text ((typekey (eql ,typekey)) ,@args) (,fn ,@args))
     (defun ,fn ,args ,@body)))


(defun uvector-equal (uv1 uv2) ;; true if same type and all data values are eql.
  (and (eql (uvector-subtag uv1)
            (uvector-subtag uv2))
       (let ((v1 (uvector-data uv1))
             (v2 (uvector-data uv2)))
         (if (or (typep v1 'cffi:foreign-pointer)
                 (typep v2 'cffi:foreign-pointer))
           (error "Should implement ~s" `(heap-vector-equal ,uv1 ,uv2))
           (and (eql (length v1) (length v2))
                (every #'eql v1 v2))))))

(defun require-sequence (x)
  (unless (or (listp x)
              (and (ccl-uvector-p x)
                   (let* ((typecode (uvector-subtag x)))
                     (declare (type (unsigned-byte 8) typecode))
                     (or (= typecode subtag-vector-header)
                         (= typecode subtag-simple-vector)
                         (and (ivector-type-p typecode)
                              (>= typecode min-cl-ivector-subtag))))))
    (report-bad-arg x 'sequence))
  x)

(defun require-gvector (uvec)
  (unless (and (ccl-uvector-p uvec)
           (gvector-type-p (uvector-subtag uvec)))
    (report-bad-arg uvec 'gvector))
  uvec)

(defun gvref (gvec index) (svref (gvector-data gvec) index))
(defun gvset (gvec index val) (setf (svref (gvector-data gvec) index) val))
(defun (setf gvref) (val gvec index) (gvset gvec index val))
(defun gvsize (gvec) (length (gvector-data gvec)))
(defun gvector-data (gvec) (require-type (uvector-data gvec) 'simple-vector))

(defun uvref (uvec index)
  (check-type uvec ccl-uvector)
  (with-uvector-data (data uvec)
    (heap-vector-uvref uvec index)
    (svref data index)))

(defun uvset (uvec index val)
  (check-type uvec ccl-uvector)
  (check-type val ccl-object)
  (with-uvector-data (data uvec)
    (heap-vector-uvset uvec index val)
    ;;; *** TEMP
    (when (ccl-bignum-p uvec)
      (check-type val (unsigned-byte 32)))
    (setf (svref data index) val)))

(defun (setf uvref) (val uvec index) (uvset uvec index val))

(defun uvsize (uvec)
  (check-type uvec ccl-uvector)
  (with-uvector-data (data uvec)
    (heap-vector-uvsize uvec)
    (length data)))



;; we need certain vectors to have a host class of their own so can use typecase,
;;  give them print methods, etc.

(def-uvector-subtype :symvector (ccl-symvector (:constructor %make-ccl-symvector) (:subtag-conser t)))
  
(deftype ccl-symbol () '(or boolean ccl-symvector))

(def-uvector-subtype :function (ccl-function (:constructor %make-ccl-function) (:subtag-conser nil))
  (bslambda () :type (or list symbol))
  ;; The native function takes 3 arguments:
  ;; (1) outer env (which is not really used but is there to provide a stack for debugging)
  ;; (2) ccl-function object, for self call and debugging
  ;; (3) the list of arguments
  (native-fn () :type (or null compiled-function)))


(def-uvector-subtype :simple-string (ccl-simple-string (:subtag-conser t)))

(defun native-string (str)
  (check-type str ccl-simple-string)
  (with-uvector-data (data str)
    (error "Should implement ~s" `(heap-vector-native-string ,str))
    (coerce data 'string)))

(def-uvector-print-text :simple-string string-print-text (str)
  (prin1-to-string (native-string str)))

(def-uvector-subtype :simple-vector (ccl-simple-vector (:subtag-conser t)))

(def-uvector-subtype :bignum (ccl-bignum (:subtag-conser t)))

(deftype ccl-integer () `(or ccl-fixnum ccl-bignum))

(def-uvector-subtype :double-float (ccl-double-float (:subtag-conser t)))

(defun dfloat-decode (loword hiword)
  (let* ((mantissa (logior (ash (ldb (byte 20 0) hiword) 32) loword))
         (exp (ldb (byte 11 20) hiword)))
    (unless (zerop exp)
      (setq mantissa (logior mantissa (ash 1 52)))
      (setq exp (1- exp)))
    (values mantissa (- exp 1074) (logbitp 31 hiword))))

(defun dfloat-encode (mantissa exp neg-p)
  (setq exp (+ 1074 exp))
  (assert (if (logbitp 52 mantissa) (not (eql exp -1)) (eql exp 0)))
  (when (logbitp 52 mantissa)
    (setq exp (1+ exp)
          mantissa (logandc2 mantissa (ash 1 52))))
  (check-type mantissa (unsigned-byte 52))
  (check-type exp (unsigned-byte 11))
  (values (ldb (byte 32 0) mantissa)
          (logior (ash exp 20) (ash mantissa -32) (if neg-p (ash 1 31) 0))))

(defun ccl-double-float (float &optional result)
  (check-type float double-float)
  (multiple-value-bind (mantissa exp sign) (integer-decode-float float)
    (multiple-value-bind (loword hiword) (dfloat-encode mantissa exp (< sign 0))
      (if result
        (with-uvector-data (vec result) :error
          (setf (svref vec 0) loword (svref vec 1) hiword)
          result)
        (make-uvector subtag-double-float (vector loword hiword))))))

(defun native-double-float (dfloat)
  (check-type dfloat ccl-double-float)
  (multiple-value-bind (mantissa exp neg-p) (dfloat-decode (uvref dfloat 0) (uvref dfloat 1))
    (let ((float (scale-float (coerce mantissa 'double-float) exp)))
      (if neg-p (- float) float))))

(def-uvector-subtype :macptr (ccl-macptr (:constructor %make-ccl-macptr) (:subtag-conser t)))

(defconstant macptr.address-cell 0) ;; this contains raw native (unsigned-byte 64).
;(defconstant macptr.domain-cell 1)
;(defconstant macptr.type-cell 2)

;(defconstant xmacptr.element-count 5)
;(defconstant xmacptr.flags-cell 3)


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
  (svref (gvector-data ptr) macptr.address-cell))
  
(defun %macptr-ptr (macptr)  ;; value as a native pointer
  (cffi:make-pointer (%macptr-value macptr)))

(defun (setf %macptr-value) (value ptr)
  (check-type ptr ccl-macptr)
  (check-type value (or integer cffi:foreign-pointer))
  (setf (svref (gvector-data ptr) macptr.address-cell)
        (if (integerp value)
          (logand #xFFFFFFFFFFFFFFFF value)
          (cffi:pointer-address value))))


;(defconstant instance.hash  0)
(defconstant instance.class-wrapper 1)
(defconstant instance.slots 2)


;(defconstant %wrapper.hash-index 1)
(defconstant %wrapper.class 2)
;(defconstant %wrapper.instance-slots 3)
;(defconstant %wrapper.class-slots 4)
;(defconstant %wrapper.slot-id->slotd 5)
;(defconstant %wrapper.slot-id-map 6)
;(defconstant %wrapper.slot-definition-table 7)
;(defconstant %wrapper.slot-id-value 8)
;(defconstant %wrapper.set-slot-id-value 9)
;(defconstant %wrapper.cpl 10)
;(defconstant %wrapper.class-ordinal 11)
;(defconstant %wrapper.cpl-bits 12)


(def-uvector-subtype :instance (ccl-instance (:constructor %make-ccl-instance) (:subtag-conser t)))

;;; ***T ODO  GET rid of all this extra typechecking once debugged

(defun instance-slot (obj slot-index)
  (check-type obj ccl-instance)
  (slot-ref (gvref obj instance.slots) slot-index))

(defun slot-ref (slot-vec index)
  (assert (eq (uvector-subtag slot-vec) subtag-slot-vector))
  (let ((val (gvref slot-vec index)))
    (if (eq val *slot-unbound-marker*)
      (unbound-slot-error slot-vec index)
      val)))

(defun instance-class (obj)
  (check-type obj ccl-instance)
  (let ((wrapper (gvref obj instance.class-wrapper)))
    (assert (istruct-typep wrapper (ccl'class-wrapper)))
    (gvref wrapper %wrapper.class)))

;(defconstant %class.direct-methods 1)			; aka specializer.direct-methods
;(defconstant %class.prototype 2)			; prototype instance
(defconstant %class.name 3)
;(defconstant %class.cpl 4)                            ; class-precedence-list
;(defconstant %class.own-wrapper 5)                    ; own wrapper (or nil)
;(defconstant %class.local-supers 6)                   ; class-direct-superclasses
;(defconstant %class.subclasses 7)                     ; class-direct-subclasses
;(defconstant %class.dependents 8)			; arbitrary dependents
;(defconstant %class.ctype 9)
;(defconstant %class.direct-slots 10)                   ; local slots
;(defconstant %class.slots 11)                          ; all slots
;(defconstant %class.info 12)                           ; cons of kernel-p, proper-name
;(defconstant %class.local-default-initargs 13)         ; local default initargs alist
;(defconstant %class.default-initargs 14)               ; all default initargs if initialized.

;; method object slots
;(defconstant %method.qualifiers 1)
;(defconstant %method.specializers 2)
(defconstant %method.function 3)
;(defconstant %method.gf 4)
(defconstant %method.name 5)
;(defconstant %method.lambda-list)

(defun ccl-class-name (obj)
  (check-type obj ccl-instance)
  (require-type (instance-slot obj %class.name) 'ccl-symbol))


(def-uvector-print-text :instance instance-print-text (obj)
  (let ((class-name (ccl-class-name (instance-class obj))))
    (cond ((eq class-name (ccl 'standard-method))
           (format nil "~a ~a"
                   (sym-print-text class-name)
                   (func-print-text (instance-slot obj %method.function))))
          (t (format nil "~a ~s slots"
                     (if class-name (sym-print-text class-name) "Unnamed Instance")
                     (uvsize (gvref obj instance.slots)))))))


;(defconstant slot-id.name 1)
(defconstant slot-id.index 2)

(def-uvector-subtype :istruct (ccl-istruct (:constructor %make-ccl-istruct) (:subtag-conser t)))

(defun make-istruct (type &rest vals)
  (%make-ccl-istruct :subtag subtag-istruct
                     :data (apply #'vector (register-istruct-cell type) vals)))

(defun istruct-type (istruct)
  (require-type (car (svref (ccl-istruct-data istruct) 0)) 'ccl-symvector))

(defun istruct-typep (obj type)
  (and (ccl-istruct-p obj) (eq (istruct-type obj) type)))


(defmethod print-object ((obj ccl-istruct) stream)
  (let ((type (istruct-type obj)))
    (if (or (eq type (ccl'pathname)) (eq type (ccl'logical-pathname)))
      (format stream "{#P~s}"
              (native-string (ccl-funcall (ccl'namestring) obj)))
      (format stream "<ISTRUCT ~a ~d slots>"
              (sym-print-text type)
              (1- (length (gvector-data obj)))))))

(def-uvector-subtype :struct (ccl-struct (:constructor %make-ccl-struct) (:subtag-conser t)))


(defconstant class-cell.name 1)

(defmethod print-object ((obj ccl-struct) stream)
  (let ((type (uvref (car (uvref obj 0)) class-cell.name)))
    (format stream "<STRUCT ~a ~d slots>"
            (sym-print-text type)
            (1- (length  (gvector-data obj))))))


(defun struct-ref (struct index)
  (check-type struct ccl-struct)
  (gvref struct index))

(defun struct-set (struct index val)
  (check-type struct ccl-struct)
  (gvset struct index val))

;; We don't need locks, but it's too hard to eliminate all references to them so just fake it.
(defconstant lockptr.size 56)
(defconstant rwlock.size 64)

(defun make-rw-lock-obj ()
  (make-uvector subtag-lock
                (vector (make-ccl-macptr (cffi:foreign-alloc :int8 :count rwlock.size :initial-element 0) $flags_DisposeRwLock)
                        (ccl 'read-write-lock)
                        0
                        nil
                        nil
                        nil)))


;; x8664 pointers to symbols or functions can be either tagged as misc or as sym/func
;; In our case, we look at it as if the pointer tag is ALWAYS sym/func, and never misc,
;;  but all the uvector stuff acccepts sym/func's.   So TYPECODE will always return
;;;  fulltag value, not subtag.  Only see the subtag if access it directly.


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
                  (eq obj *unbound-function*)
                  (eq obj *macro-apply-code*))
              fulltag-immediate)
             (t (error "not a CCL-VM object: ~s" obj))))))

(deftype ccl-object () `(or ccl-fixnum boolean list character single-float
                            ccl-uvector
                            (eql ,*unbound-marker*)
                            (eql ,*slot-unbound-marker*)
                            (eql ,*illegal-marker*)
                            (eql ,*unbound-function*)
                            (eql ,*macro-apply-code*)))

(declaim (inline ccl-object-p))
(defun ccl-object-p (obj) (typep obj 'ccl-object))

(defun lisptag (obj) ;; returns 3 bit typecode
  (logand 7 (fulltag obj)))

(defun typecode (obj)
  (if (ccl-uvector-p obj)
    (uvector-subtag obj)
    (if (eq obj t)
      subtag-symvector ;; be consistent
      (lisptag obj))))

(defun native (ccl-obj)
  (typecase ccl-obj
    ((or boolean ccl-fixnum single-float character) ccl-obj)
    (cons (let ((car (native (car ccl-obj))) (cdr (native (cdr ccl-obj))))
            (if (and (eq car (car ccl-obj)) (eq cdr (cdr ccl-obj)))
              ccl-obj
              (cons car cdr))))
    (ccl-uvector
     ;; These are all ccl-uvector subtypes, could use typecase!
     (let ((subtag (uvector-subtag ccl-obj)))
       (cond ((eq subtag subtag-simple-string) (native-string ccl-obj))
             ((eq subtag subtag-double-float) (native-double-float ccl-obj))
             ((eq subtag subtag-macptr) (%macptr-ptr ccl-obj))
             ((eq subtag subtag-bignum) (native-integer ccl-obj))
             ((eq subtag subtag-symvector) (native-symbol ccl-obj))
             ((eq subtag subtag-simple-vector) (gvector-data ccl-obj))
             (t (error "Don't know how to nativize ~s" ccl-obj)))))
    (t ccl-obj)))

(defun ccl (obj)
  (typecase obj
    (ccl-uvector obj)
    (simple-base-string (ccl-string obj))
    ((or boolean ccl-fixnum single-float character) obj)
    (symbol (ccl-symbol obj))
    (integer (ccl-bignum obj))
    (double-float (ccl-double-float obj))
    (number (ccl-number obj))
    (simple-vector (ccl-vector obj))
    (cons (cons (ccl (car obj)) (ccl (cdr obj)))) ;; hope it's not circular...
    (cffi:foreign-pointer (make-ccl-macptr obj))
    (t (error "Don't know how to cclify ~s" obj))))
        
;; For interactive use
(defun ccall (sym-or-func &rest args)
  (ccl-funcall (ccl sym-or-func) (mapcar #'ccl args)))




(defun ccl-number (obj)
  (typecase obj
    ((or ccl-fixnum single-float) obj)
    (integer (ccl-bignum obj))
    (t (error "~s conversion not implemented yet" obj))))

(defun ccl-string (obj)
  (make-ccl-simple-string :subtag subtag-simple-string
                          :data (coerce obj 'simple-vector)))

(defun ccl-vector (obj)
  (error "ccl-vector not implemented yet for ~s" obj))

(defparameter *uvector-subtag-type-names*
  (let ((vec (make-array 256 :initial-contents *uvector-subtag-typekeys*)))
    ;; a few renamings
    (setf (svref vec subtag-struct) 'structure)
    (setf (svref vec subtag-istruct) 'internal-structure)
    (setf (svref vec subtag-simple-string) 'simple-base-string)
    (setf (svref vec subtag-signed-8-bit-vector) 'simple-signed-byte-vector)
    (setf (svref vec subtag-unsigned-8-bit-vector)  'simple-unsigned-byte-vector)
    (setf (svref vec subtag-signed-16-bit-vector) 'simple-signed-word-vector)
    (setf (svref vec subtag-unsigned-16-bit-vector)  'simple-unsigned-word-vector)
    (setf (svref vec subtag-signed-32-bit-vector) 'simple-signed-long-vector)
    (setf (svref vec subtag-unsigned-32-bit-vector) 'simple-unsigned-long-vector)
    (setf (svref vec subtag-signed-64-bit-vector) 'simple-signed-doubleword-vector)
    (setf (svref vec subtag-unsigned-64-bit-vector) 'simple-unsigned-doubleword-vector)
    vec))

(defun uvector-type-name (obj)
  (or (svref *uvector-subtag-type-names* (uvector-subtag obj))
      (error "Bogus subtag in ~s" obj)))

;; NOTE this doesn't distinguish the two types of locks, lap-%type-of does.
(defun %type-name-of (obj)
  (etypecase obj
    (ccl-fixnum 'fixnum)
    (null 'null)
    (list 'cons)
    (character 'character)
    (single-float 'short-float)
    ((eql t) 'symbol)
    (ccl-symvector 'symbol)
    (ccl-function (ccl-function-type-name obj))
    (ccl-uvector (uvector-type-name obj))
    (t (cond ((or (eq obj *unbound-marker*)
                  (eq obj *slot-unbound-marker*)
                  (eq obj *illegal-marker*)
                  (eq obj *unbound-function*)
                  (eq obj *macro-apply-code*))
              'immediate)
             (t (cerror "not a CCL-VM object: ~s" "return 'bogus" obj)
                'bogus)))))
