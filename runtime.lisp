(in-package :ccl-vm)

;;; TODO: ** Are all args to CCL quoted?  Maybe it should be a macro

;; Instead of putting lap functions in compiled files, put them directly here in the runtime

(defmacro deflapfunction (name args &body body)
  (let ((lap-fn (intern (concatenate 'string "LAP-" (string name)) *native-package*)))
    `(progn
       (defun ,lap-fn ,args ,@body)
       (let ((_name (ccl ',name))
             (_fn (cons-ccl-function)))
         (setf (ccl-function-bslambda _fn) 'lap) ;; could cons something up which calls the fn?
         (setf (ccl-function-name _fn) _name)
         (setf (ccl-function-native-fn _fn) #',lap-fn)
         (setf (sym-func _name) _fn)))))

(deflapfunction fout (string &rest values)
  (fresh-line *trace-output*)
  (apply #'format *trace-output* (native-string string) values))

;;; ** TODO: do in lap for now so can move on, but need to figure this out.

#+CCL
(deflapfunction soname-from-mach-header (header)
  (setq header (%macptr-ptr header))
  (do* ((p (ccl::%inc-ptr header
                     #+64-bit-target (ccl::record-length :mach_header_64)
                     #-64-bit-target (ccl::record-length :mach_header))
           (ccl::%inc-ptr p (ccl::pref p :load_command.cmdsize)))
        (i 0 (1+ i))
        (n (ccl::pref header
                 #+64-bit-target :mach_header_64.ncmds
                 #-64-bit-target :mach_header.ncmds)))
       ((= i n))
    (when (= #$LC_ID_DYLIB (ccl::pref p :load_command.cmd))
      (return (ccl (ccl::%get-cstring (ccl::%inc-ptr p (ccl::record-length :dylib_command))))))))

(deflapfunction cvm-gvectorp (obj)
  (and (ccl-uvector-p obj)
       (gvector-type-p (ccl-uvector-subtag obj))))

;; used e.g. by %make-rwlock-ptr
;;; TODO: the point is for this to be weak and finalizable
(defparameter *gcable-pointers* nil)
(deflapfunction set-%gcable-macptrs% (macptr)
  (push macptr *gcable-pointers*)
  macptr)

(deflapfunction %setf-macptr-to-object (macptr obj)
  ;; So far only used for (%current-tcr) which is 0.
  ;; if need be, can start using the *fake-address  stuff below.
  (if (typep obj 'ccl-fixnum)
    (setf (%macptr-value macptr) (ash obj fixnum-shift))
    (error "Don't know how to %set-macptr-to-object ~s" obj)))

(deflapfunction %set-object (macptr offset obj)
  (if (typep obj 'ccl-fixnum)
    (setf (cffi:mem-ref (%macptr-ptr macptr) :int64 offset) (ash obj fixnum-shift))
    (error "Don't know how to %set-object ~s" obj)))

(deflapfunction %get-object (macptr offset)
  (let ((addr (cffi:mem-ref (%macptr-ptr macptr) :int64 offset)))
    (or (and (eql (logand addr (1- (ash 1 fixnum-shift))) lisptag-fixnum)
             (let ((val (ash addr (- fixnum-shift))))
               (and (typep val 'ccl-fixnum) val)))
        (error "Don't know how to %get-object ~s" addr))))

(deflapfunction cvm-foreign-size (sym)
  (let ((key (sym-keyword sym)))
    #+ccl (ccl::%foreign-type-or-record-size key :bytes)
    #-ccl (error "Don't know how to get record size of ~s" key)))


(deflapfunction cvm-access-foreign-record (ccl-ptr accessors bit-offset)
  (cassert (eql 0 bit-offset)) ;; not needed
  (let* ((ptr (cffi:make-pointer (%macptr-value ccl-ptr)))
         (accessor-names (loop for sym in accessors
                           do (assert (eq (sym-pkg sym) *keyword-pkg*))
                           collect "."
                           collect (sym-native-pname sym)))
         (accessor-name (apply #'concatenate 'string (cdr accessor-names)))
         (accessor (intern accessor-name :keyword)))
    #-ccl (error "Don't know how to access foreign record ~s" accessor)
    #+ccl (eval `(ccl:pref ',ptr ,accessor))))

(defvar *CFFI-CONSTANTS* (make-package "CFFI-CONSTANTS" :use nil))

(defconstant u32-mask #xFFFFFFFF)

(declaim (inline u32-sign-extend))
(defun u32-sign-extend (word)
  ;(declare (type (unsigned-byte 32) word))
  ;;; *** TODO: REMOVE
  (check-type word (unsigned-byte 32))
  (if (logbitp 31 word) (logior word (ash -1 -32)) word))

(deflapfunction cvm-os-constant (sym)
  #-ccl (error "Don't know how to get OS constant ~s" sym)
  #+ccl
  (let ((sym (intern (sym-native-pname sym) *CFFI-CONSTANTS*)))
    (unless (boundp sym)
      (ccl::load-os-constant sym))
    (symbol-value sym)))

(deflapfunction called-for-mv-p () t)

(deflapfunction %fixnum-truncate (dividend divisor)
  (multiple-value-bind (q r) (truncate dividend divisor)
    (values (ccl q) (ccl r))))

;;; Return the (possibly truncated) 32-bit quotient and remainder
;;; resulting from dividing hi:low by divisor.
(deflapfunction %floor (num-high num-low divisor)
  (check-type num-high (unsigned-byte 32))
  (check-type num-low (unsigned-byte 32))
  (check-type divisor (unsigned-byte 32))
  (multiple-value-bind (q r) (floor (logior (ash num-high 32) num-low) divisor)
    (values (logand q u32-mask) r)))
    
(deflapfunction %ashr (digit count)
  ;(declare (type (unsigned-byte 32) digit) (type (unsigned-byte 8) count))
  ;;; TODO REMOVE
  (check-type digit (unsigned-byte 32))
  (check-type count (unsigned-byte 8))
  (ash (u32-sign-extend digit) (- count)))


(deflapfunction %ashl (digit count)
  ;(declare (type (unsigned-byte 32) digit) (type (unsigned-byte 8) count))
  ;;; TODO REMOVE
  (check-type digit (unsigned-byte 32))
  (check-type count (unsigned-byte 8))
  (logand (ash digit count) u32-mask))

;;; single digit as a fixnum.  Otherwise, if it's a two-digit-bignum
;;; and the two words of the bignum can be represented in a fixnum,
;;; return that fixnum; else return nil.
(deflapfunction %maybe-fixnum-from-one-or-two-digit-bignum (bignum)
  (assert (eq num-fixnum-bits 61)) ;; stop pretending...
  (let ((vec (ccl-uvector-data bignum)))
    (case (length vec)
      (1 (u32-sign-extend (svref vec 0)))
      (2 (let* ((low (svref vec 0))
                (high (svref vec 1))
                (too-high (ash high -28))) ;; sign bit + extra
           (when (or (eql too-high 0) (eql too-high #xF))
             (logior (ash (u32-sign-extend high) 32) low))))
      (t nil))))

(deflapfunction %truncate-short-float->fixnum (f) (truncate f))

(deflapfunction %fixnum-to-bignum-set (bignum fixnum)
  ;; bignum is a two-digit bignum
;;; The caller has allocated a two-digit bignum (quite likely on the stack).
;;; If we can fit in a single digit (if the high word is just a sign
;;; extension of the low word), truncate the bignum in place (the
;;; trailing words should already be zeroed.
  (let ((high1 (ash fixnum -31))
        (low (logand fixnum u32-mask)))
    (if (or (eql high1 0) (eql high1 -1))
      (setf (ccl-uvector-data bignum) (vector (logand fixnum u32-mask)))
      (let ((vec (ccl-uvector-data bignum)))
        (setf (svref vec 0) low)
        (setf (svref vec 1) (logand (ash high1 -1) u32-mask)))))
  fixnum)

(defun ccl-bignum (bignum)
  (let* ((bits (integer-length bignum)) ;; bits not including sign
         (size (ceiling (1+ bits) 32))
         (vec (make-array size)))
    (loop for i from 0 below size
      do (setf (svref vec i) (logand bignum u32-mask))
      do (setq bignum (ash bignum (- 32))))
    (make-ccl-bignum :subtag subtag-bignum :data vec)))

(defun native-integer (ccl-number)
  (if (fixnump ccl-number)
    ccl-number
    (if (ccl-bignum-p ccl-number)
      (let* ((vec (ccl-uvector-data ccl-number))
             (end (1- (length vec)))
             (bignum (if (logbitp 31 (svref vec end)) -1 0)))
        (loop for i from end downto 0
          do (setq bignum (logior (ash bignum 32) (svref vec i))))
        bignum)
      (error "Not an integer ~s" ccl-number))))


(deflapfunction %get-gc-count () 17)

(defparameter *fake-addresses-vector* (make-array 100 :fill-pointer 0))
(DEFVAR *FAKE-ADDRESS-TYPES* ())
;; there is an EQ has with functions, so far only #'pathname-encoding-name.

(deflapfunction strip-tag-to-fixnum (obj)
  ;; it has eliminated fixnum, instance, symbol.
  (PUSHNEW (class-of obj) *FAKE-ADDRESS-TYPES*)
  (if (characterp obj)
    (char-code obj)
    (if (typep obj 'single-float)
      (multiple-value-bind (m exp sign) (integer-decode-float obj)
        (let ((uexp (+ exp (ash 1 12))))
          ;; Just put them tegether in any consistent way
          (check-type m (unsigned-byte 32))
          (check-type uexp (unsigned-byte 12))
          (logior m
                  (ash uexp 32)
                  (if (eql sign -1) (ash 1 (+ 32 12)) 0))))
      (vector-push-extend obj *fake-addresses-vector*))))


;; ccl has fast-mod, why doesn't it have an optimizer to use it??
;; sbcl does use this.
(deflapfunction fast-mod (num divisor)
  ;;(declare (type (unsigned-byte #.(1- num-fixnum-bits)) num divisor))
  ;;; TODO: remove
  (check-type num (unsigned-byte #.(1- num-fixnum-bits)))
  (check-type divisor (unsigned-byte #.(1- num-fixnum-bits)))
  (mod num divisor))


(deflapfunction fast-mod-3 (number divisor recip)
  (let ((res
         (let* ((high (ash (* number recip) -61))
                (low (logand (1- (ash 1 61)) (* high divisor)))
                (result (- number low divisor)))
           (if (logbitp 60 result)
      (+ result divisor)
      result)))
        )
  ;;; TODO: remove
  ;; ash shift LEFT
  ;; Values as they appear in registers
    (unless (eq res (ccl::fast-mod-3 number divisor recip))
      (break "fast-mod-3 ~s ~s ~s our ~s ccl ~s"
             number divisor recip res (ccl::fast-mod-3 number divisor recip)))
    res))




;; We don't need locks, but it's too hard to eliminate all references to them so just fake it.
(defconstant lockptr.size 56)
(defconstant rwlock.size 64)

(defmethod print-uvector-data ((type (eql :lock)) lock stream)
  (let ((lockv (ccl-uvector-data lock)))
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

(defun kernel-import-new-recursive-lock ()
  (make-ccl-macptr (cffi:foreign-alloc :int8 :count lockptr.size :initial-element 0)))

(defun kernel-import-rwlock-new ()
  (make-ccl-macptr (cffi:foreign-alloc :int8 :count rwlock.size :initial-element 0)))

(defun kernel-import-wait-on-semaphore (address seconds millis)
  (declare (ignore address seconds millis))
  0)

(defun kernel-import-signal-semaphore (semaphore)
  (declare (ignore semaphore))
  0)

(defun kernel-import-wait-for-signal (signo seconds millis)
  (declare (ignore signo seconds millis))
  0)

(deflapfunction %get-spin-lock (spin) spin)

(cffi:defcfun (ff-dlsym "dlsym") :uint64
  (handle :uint64)
  (name (:pointer :char)))

(defconstant u64-mask #xFFFFFFFFFFFFFFFF)
(defconstant RTLD_DEFAULT (logand u64-mask -2))

;; Used by foreign-symbol-entry and foreign-symbol-address. 
(defun kernel-import-findsymbol (handle name)
  (check-type handle ccl-macptr)
  (check-type name ccl-macptr)
  (let* ((hval (%macptr-value handle))
         (name-ptr (cffi:make-pointer (%macptr-value name))))
    (when (eq hval 0) (setq hval RTLD_DEFAULT))
    (let ((val (ff-dlsym hval name-ptr)))
      (when (and (eql val 0) (eql (cffi:mem-ref name-ptr :char 0) #\_))
        (setq val (ff-dlsym hval (cffi:inc-pointer name-ptr 1))))
      (when (eql val 0)
        (error "Can't find symbol ~s" (cffi:foreign-string-to-lisp name-ptr)))
      val)))

;; Need to make %ALL-PACKAGES-LOCK% early, so that we can casually
;; do SET-PACKAGE in cold load functions.
(let ((lock (make-ccl-macptr (%macptr-value (kernel-import-rwlock-new)) $flags_DisposeRwLock)))
  (setf (sym-value (ccl '%all-packages-lock%)) (gvector subtag-lock lock (ccl 'read-write-lock) 0 nil nil nil))
  (setf (sym-value (ccl '%system-locks%)) (gvector subtag-population 0 0 (list lock))))

(defconstant node-size 8)

;; No threads, no big deal!  Except we have to reverse-engineer the offset
(defun uvector-offset-to-cell-index (uvec offset)
  (cassert (gvector-type-p (ccl-uvector-subtag uvec)))
  (let* ((bytes (+ offset fulltag-misc))
         (index (1- (ash bytes -3)))) ;;subtract one for header node
    (assert (zerop (logand bytes 7)))
    index))



(deflapfunction %store-node-conditional (node-offset object old new)
  (etypecase object
    (ccl-uvector (let ((index (uvector-offset-to-cell-index object node-offset)))
                   (when (eq old (svref (uvector object) index))
                     (setf (svref (uvector object) index) new)
                     T)))))


(deflapfunction %set-hash-table-vector-key-conditional (node-offset vector old new)
  (let ((vec (ccl-uvector-data vector))
        (index (uvector-offset-to-cell-index vector node-offset)))
    (when (eq old (svref vec index))
      (setf (svref vec index) new)
      T)))


;;; THE x8664 code for this returns  expected-val regardless of whether succeeded or not!!!  That's gotta be a bug.
(deflapfunction %ptr-store-fixnum-conditional (ptr expected-val new-val)
  (let* ((raw-expected (ash expected-val fixnum-shift))
         (raw-new (ash new-val fixnum-shift))
         (raw-ptr (%macptr-ptr ptr))
         (raw-old (cffi:mem-ref raw-ptr :int64)))
    (cond ((eql raw-expected raw-old)
           (setf (cffi:mem-ref raw-ptr :int64) raw-new)
           expected-val)
          (t
           (assert (eql 0 (logand (1- (ash 1 fixnum-shift)) raw-old)))
           (ash raw-old (- fixnum-shift))))))

(deflapfunction %atomic-incf-node (by object offset)
  (etypecase object
    (ccl-uvector (let ((index (uvector-offset-to-cell-index object offset)))
                   (incf (svref (uvector object) index) by)))))


  


(deflapfunction closure-function (func) (ccl-closure-function func))

(deflapfunction %symptr->symbol (symvector)
  (if (eq symvector *nil-sym*) nil
    (if (eq symvector *t-sym*) t
      (require-type symvector 'ccl-symvector))))

(deflapfunction %set-hash-table-vector-key (vector index value)
  (setf (svref (ccl-uvector-data vector) index) value))


(deflapfunction %string-hash (start str len)
  (check-type str ccl-simple-base-string)
  (loop with vec = (ccl-uvector-data str)
    for hash = 0 then (logxor (logior (logand (ash hash 5) u32-mask) (ash hash -27))
                              (char-code (aref vec index)))
    for index from start below len
    finally (return hash)))

(deflapfunction %pname-hash (str len)
  (lap-%string-hash 0 str len))

(deflapfunction eql (x y)
  (or (eq x y)
      (and (ccl-uvector-p x)
           (ccl-uvector-p y)
           (let ((subtag (ccl-uvector-subtag x)))
             (and (eq subtag (ccl-uvector-subtag y))
                  (cond ((eq subtag subtag-macptr)
                         (eql (%macptr-value x) (%macptr-value y)))
                        ((logbitp subtag numeric-subtag-mask)
                         (let ((xv (ccl-uvector-data x))
                               (yv (ccl-uvector-data y)))
                           (and (eql (length xv) (length yv))
                                (every #'lap-eql xv yv))))
                        (t nil)))))))


(deflapfunction equal (x y)
  (or (eq x y)
      (cond ((consp x)
             (and (consp y)
                  (lap-equal (car (the cons x)) (car (the cons y)))
                  (lap-equal (cdr (the cons x)) (cdr (the cons y)))))
            ((and (ccl-simple-base-string-p x) (ccl-simple-base-string-p y))
             (let ((xv (ccl-uvector-data x))
                   (yv (ccl-uvector-data y)))
               (and (eql (length xv) (length yv)) (every #'eql xv yv))))
            (t
             (let ((tag (fulltag x)))
               (and (eq tag (Fulltag y))
                    (cond ((eq tag fulltag-misc)
                           (ccl-funcall (ccl 'hairy-equal) x y))
                          (t nil))))))))
