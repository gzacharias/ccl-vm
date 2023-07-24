(in-package :ccl-vm)

(deftype ccl-fixnum () `(signed-byte ,num-fixnum-bits))

(defparameter *subtag-consers* ())
;;; *** TODO, need to rename this.

;; a simple vector for everything, until there's a good reason not to.
(defstruct (ccl-uvector (:constructor %raw-make-uvector) (:conc-name uvector-))
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
  (let ((type (subtag-typekey (uvector-subtag obj))))
    (format stream "<~s ~s " (type-of obj) type)
    (print-uvector-data type obj stream)
    (format stream ">")))

(defmethod print-uvector-data ((type t) obj stream)
  (format stream "~s elts" (length (uvector-data obj))))

(defun uvector-equal (uv1 uv2) ;; true if same type and all data values are eql.
  (and (eql (uvector-subtag uv1)
            (uvector-subtag uv2))
       (let ((v1 (uvector-data uv1))
             (v2 (uvector-data uv2)))
         (and (eql (length v1) (length v2))
              (every #'eql v1 v2)))))

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

(defun gvref (uvec index)
  (uvref (require-gvector uvec) index))

(defun gvset (uvec index val)
  (uvset (require-gvector uvec) index val))

(defun uvref (uvec index)
  (check-type uvec ccl-uvector)
  (svref (uvector-data uvec) index))

(defun uvset (uvec index val)
  (check-type uvec ccl-uvector)
  (check-type val ccl-object)
  (setf (svref (uvector-data uvec) index) val))

(defun (setf uvref) (val uvec index)
  (uvset uvec index val))

(defun uvsize (uvec)
  (check-type uvec ccl-uvector)
  (length (uvector-data uvec)))

;; we need certain vectors to have a host class of their own so can use typecase,
;;  give them print methods, etc.

(def-uvector-subtype :symbol (ccl-symvector (:constructor %make-ccl-symvector) (:subtag-conser t)))
  
(deftype ccl-symbol () '(or boolean ccl-symvector))

(def-uvector-subtype :function (ccl-function (:constructor %make-ccl-function) (:subtag-conser nil))
  (bslambda () :type (or list symbol))
  ;; The native function takes 3 arguments:
  ;; (1) outer env (which is not really used but is there to provide a stack for debugging)
  ;; (2) ccl-function object, for self call and debugging
  ;; (3) the list of arguments
  (native-fn () :type (or null compiled-function)))


(def-uvector-subtype :simple-string (ccl-simple-base-string (:subtag-conser t)))

(defun native-string (str)
  (check-type str ccl-simple-base-string)
  (coerce (uvector-data str) 'string))

(defmethod print-object ((str ccl-simple-base-string) stream)
  (assert (eq (uvector-subtag str) subtag-simple-string))
  (format stream "<STRING ")
  (print-string-data str stream)
  (format stream ">"))

(defmethod print-uvector-data ((type (eql :simple-string)) str stream) (print-string-data str stream))

(defun print-string-data (str stream)
  (prin1 (native-string str) stream))

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
  (svref (uvector-data ptr) macptr.address-cell))
  
(defun %macptr-ptr (macptr)  ;; value as a native pointer
  (cffi:make-pointer (%macptr-value macptr)))

(defun (setf %macptr-value) (value ptr)
  (check-type ptr ccl-macptr)
  (check-type value (or integer cffi:foreign-pointer))
  (setf (svref (uvector-data ptr) macptr.address-cell)
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
  (slot-ref (uvref obj instance.slots) slot-index))

(defun slot-ref (slot-vec index)
  (assert (eq (uvector-subtag slot-vec) subtag-slot-vector))
  (let ((val (uvref slot-vec index)))
    (if (eq val *slot-unbound-marker*)
      (unbound-slot-error slot-vec index)
      val)))

(defun instance-class (obj)
  (check-type obj ccl-instance)
  (let ((wrapper (uvref obj instance.class-wrapper)))
    (assert (istruct-typep wrapper (ccl'class-wrapper)))
    (uvref wrapper %wrapper.class)))

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


(defmethod print-object ((obj ccl-instance) stream)
  (assert (eq (uvector-subtag obj) subtag-instance))
  (let ((class-name (ccl-class-name (instance-class obj))))
    (cond ((eq class-name (ccl 'standard-method))
           (princ "<STANDARD-METHOD " stream)
           (print-function-data (instance-slot obj %method.function) stream)
           (princ ">" stream))
          (t
           (princ "<INSTANCE " stream)
           ;(print-instance-data :instance obj stream)
           (princ ">" stream)))))

(defmethod print-uvector-data ((type (eql :instance)) obj stream) (print-instance-data obj stream))

(defun print-instance-data (obj stream)
  (let ((class-name (ccl-class-name (instance-class obj))))
    (print-symbol-data class-name stream)
    (format stream " ~s slots" (length (svref (uvector-data obj) instance.slots)))))

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
  (princ "<ISTRUCT " stream)
  (print-symbol-data (istruct-type obj) stream)
  (format stream " ~d slots>" (1- (length (uvector-data obj)))))



(def-uvector-subtype :struct (ccl-struct (:constructor %make-ccl-struct) (:subtag-conser t)))

(defun struct-ref (struct index)
  (check-type struct ccl-struct)
  (uvref struct index))

(defun struct-set (struct index val)
  (check-type struct ccl-struct)
  (uvset struct index val))

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

(defmethod print-uvector-data ((type (eql :lock)) lock stream)
  (let ((lockv (uvector-data lock)))
    (format stream "KIND ~s WRITER ~s "
            (if (eq (svref lockv 1) (ccl 'recursive-lock))
              'recursive-lock
              (if (eq (svref lockv 1) (ccl 'read-write-lock))
                'read-write-lock
                (svref lockv 1)))
            (svref lockv 2))
    (cond ((eq (svref lockv 1) (ccl 'recursive-lock))
           (let ((ptr (%macptr-ptr (svref lockv 0))))
             (format stream "LOCKPTR -> avail: ~s owner x~x count ~s signal ~s waiting ~s spinlock ~s"
                     (cffi:mem-ref ptr :uint64 0)
                     (cffi:mem-ref ptr :uint64 8)
                     (cffi:mem-ref ptr :uint64 16)
                     (cffi:mem-ref ptr :uint64 24)
                     (cffi:mem-ref ptr :uint64 32)
                     (cffi:mem-ref ptr :uint64 48))))
          ((eq (svref lockv 1) (ccl 'read-write-lock))
           (let ((ptr (%macptr-ptr (svref lockv 0))))
             (format stream "RWLOCK -> spin: ~s state ~s blocked writers ~s readers ~s writer ~s signals reader ~s writer ~s"
                     (cffi:mem-ref ptr :uint64 0)
                     (cffi:mem-ref ptr :uint64 8)
                     (cffi:mem-ref ptr :uint64 16)
                     (cffi:mem-ref ptr :uint64 24)
                     (cffi:mem-ref ptr :uint64 32)
                     (cffi:mem-ref ptr :uint64 40)
                     (cffi:mem-ref ptr :uint64 48))))
          (t (format stream "Unknown ptr ~s" (svref lockv 0))))))


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
      subtag-symbol ;; be consistent
      (lisptag obj))))

(defun ccl (obj)
  (typecase obj
    (ccl-uvector obj)
    (simple-base-string (ccl-string obj))
    ((or null ccl-fixnum single-float character) obj)
    (symbol (ccl-symbol obj))
    (integer (ccl-bignum obj))
    (number (ccl-number obj))
    (simple-vector (ccl-vector obj))
    (cons (cons (ccl (car obj)) (ccl (cdr obj)))) ;; hope it's not circular...
    (cffi:foreign-pointer (make-ccl-macptr obj))
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
