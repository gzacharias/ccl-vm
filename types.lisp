(in-package :ccl-vm)

(deftype ccl-fixnum () `(signed-byte ,num-fixnum-bits))

(defparameter *subtag-consers* ())

;; a simple vector for everything, until there's a good reason not to.
(defstruct ccl-uvector
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


(defmethod print-object ((obj ccl-uvector) stream)
  (let ((type (subtag-typekey (ccl-uvector-subtag obj))))
    (format stream "<~s ~s " (type-of obj) type)
    (print-uvector-data type obj stream)
    (format stream ">")))

(defmethod print-uvector-data ((type t) obj stream)
  (format stream "~s elts" (length (ccl-uvector-data obj))))

(declaim (inline uvector))
(defun uvector (obj)
  (ccl-uvector-data (if (typep obj 'boolean) (sym-symvector obj) obj)))

(defun uvector-equal (uv1 uv2) ;; true if same type and all data values are eql.
  (and (eql (ccl-uvector-subtag uv1)
            (ccl-uvector-subtag uv2))
       (let ((v1 (ccl-uvector-data uv1))
             (v2 (ccl-uvector-data uv2)))
         (and (eql (length v1) (length v2))
              (every #'eql v1 v2)))))

(defun gvref (uvec index)
  (unless (and (ccl-uvector-p uvec)
           (gvector-type-p (ccl-uvector-subtag uvec)))
    (report-bad-arg uvec 'gvector))
  (uvref uvec index))

(defun gvset (uvec index val)
  (unless (and (ccl-uvector-p uvec)
               (gvector-type-p (ccl-uvector-subtag uvec)))
    (report-bad-arg uvec 'gvector))
  (uvset uvec index val))

(defun uvref (uvec index)
  (check-type uvec ccl-uvector)
  (svref (ccl-uvector-data uvec) index))

(defun uvset (uvec index val)
  (check-type uvec ccl-uvector)
  (check-type val ccl-object)
  (setf (svref (ccl-uvector-data uvec) index) val))

(defun (setf uvref) (val uvec index)
  (uvset uvec index val))

(defun uvsize (uvec)
  (check-type uvec ccl-uvector)
  (length (ccl-uvector-data uvec)))

;; we need certain vectors to have a host class of their own so can use typecase,
;;  give them print methods, etc.

(def-uvector-subtype :symbol (ccl-symvector (:constructor %make-ccl-symvector) (:subtag-conser t)))
  
(deftype ccl-symbol () '(or boolean ccl-symvector))

(def-uvector-subtype :function (ccl-function (:constructor %make-ccl-function) (:subtag-conser nil))
  (bslambda () :type (or list (eql lap)))
  ;; The native function takes 3 arguments:
  ;; (1) outer env (which is not really used but is there to provide a stack for debugging)
  ;; (2) ccl-function object, for self call and debugging
  ;; (3) the list of arguments
  (native-fn () :type (or null compiled-function)))


(def-uvector-subtype :simple-string (ccl-simple-base-string (:subtag-conser t)))

(defun native-string (str)
  (check-type str ccl-simple-base-string)
  (coerce (ccl-uvector-data str) 'string))

(defmethod print-uvector-data ((type (eql :simple-string)) obj stream)
  (prin1 (native-string obj) stream))

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
  (svref (uvector ptr) macptr.address-cell))
  
(defun %macptr-ptr (macptr)  ;; value as a native pointer
  (cffi:make-pointer (%macptr-value macptr)))

(defun (setf %macptr-value) (value ptr)
  (check-type ptr ccl-macptr)
  (check-type value (or integer cffi:foreign-pointer))
  (setf (svref (uvector ptr) macptr.address-cell)
        (if (integerp value)
          (logand #xFFFFFFFFFFFFFFFF value)
          (cffi:pointer-address value))))


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
                  (eq obj *unbound-function*))
              fulltag-immediate)
             (t (error "not a CCL-VM object: ~s" obj))))))

(deftype ccl-object () `(or ccl-fixnum boolean list character single-float
                            ccl-uvector
                            (eql ,*unbound-marker*)
                            (eql ,*slot-unbound-marker*)
                            (eql ,*illegal-marker*)
                            (eql ,*unbound-function*)))

(declaim (inline ccl-object-p))
(defun ccl-object-p (obj) (typep obj 'ccl-object))

(defun lisptag (obj) ;; returns 3 bit typecode
  (logand 7 (fulltag obj)))

(defun typecode (obj)
  (let ((tag (lisptag obj)))
    (if (eq tag lisptag-misc)
      (ccl-uvector-subtag obj)
      ;; It doesn't seem to work otherwise...
      (if (eq tag lisptag-symbol)
        subtag-symbol
        (if (eq tag lisptag-function)
          subtag-function
          tag)))))

(defun ccl (obj)
  (typecase obj
    (ccl-uvector obj)
    (simple-base-string (ccl-string obj))
    ((or null ccl-fixnum single-float character) obj)
    (symbol (ccl-symbol obj))
    (integer (ccl-bignum obj))
    (number (ccl-number obj))
    (simple-vector (ccl-vector obj))
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

#|
;;;; *** TODO: another weird thing to figure out and bootstrap
(defvar %find-classes% (make-hash-table :test 'eq))

(defun find-class-cell (name create?)
  (let ((cell (gethash name %find-classes%)))
    (or cell
        (and create?
             ;; (%istruct 'class-cell name nil '%make-instance nil)
             (setf (gethash name %find-classes%)
                   (make-ccl-uvector :subtag subtag-istruct
                                     :data (vector
                                            (register-istruct-cell (ccl 'class-cell))
                                            name
                                            nil
                                            (ccl '%make-instance)
                                            nil)))))))

|#
