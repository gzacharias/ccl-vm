(in-package :ccl-vm)

;;; TODO: ** Are all args to CCL quoted?  Maybe it should be a macro

;; Instead of putting lap functions in compiled files, put them directly here in the runtime

(defmacro deflapfunction (name args-or-lap-name &body body)
  (let ((lap-fn (if (listp args-or-lap-name)
                  (intern (concatenate 'string "LAP-" (string name)) *native-package*)                  
                  (require-type args-or-lap-name 'symbol)))
        (env-p nil))
    `(progn
       ,(if (listp args-or-lap-name)
          (if (setq env-p (member '&environment args-or-lap-name))
            (let ((inner-args (butlast args-or-lap-name 2))
                  (outer-args (list (cadr env-p) (gensym) (gensym))))
              (assert (eql (length env-p) 2))
              `(defun ,lap-fn ,outer-args
                 (declare (ignore ,(cadr outer-args))) ;; self
                 (destructuring-bind ,inner-args ,(caddr outer-args)
                   ,@body)))
            `(defun ,lap-fn ,args-or-lap-name ,@body))
          (assert (null body)))
       (let ((_name (ccl-symbol ',name))
             (_fn (cons-ccl-function)))
         (setf (ccl-function-data _fn) (vector _name 0)) ;; could add arg info...
         (setf (ccl-function-bslambda _fn) ',(if env-p 'lap-with-env 'lap))
         (setf (ccl-function-native-fn _fn) #',lap-fn)
         (setf (sym-func _name) _fn)))))

(deflapfunction fout (string &rest values)
  (fresh-line *trace-output*)
  (apply #'format *trace-output* (native-string string) values))

(deflapfunction fdescribe (obj)
  (describe obj))

(deflapfunction fbreak (str &rest args)
  (apply #'break (native-string str) args)
  )

(deflapfunction %fasload (namestring)
  (pretend-fasload (native-string namestring)))

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
       (gvector-type-p (uvector-subtag obj))))


;; used e.g. by %make-rwlock-ptr
;;; TODO: the point is for this to be weak and finalizable
(defparameter *gcable-pointers* nil)
(deflapfunction set-%gcable-macptrs% (macptr)
  (push macptr *gcable-pointers*)
  macptr)

(deflapfunction %revive-macptr (macptr)
  (when (eq (uvector-subtag macptr) subtag-dead-macptr)
    ;; Gets a bit complicated because ccl-macptr is a structure type in the host,
    (error "Nobody expects a dead macptr")))

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

;; 'weak-gc-method  'batch-flag 'all-areas 'tenured-area 'statically-linked 'host-platform 'batch-flag
;; static-cons-area free-static-conses ret1valaddr 'ppc::altivec-present 'stack-size 'default-allocation-quantum
;; 'oldest-ephemeral

;; exception-lock, area-lock
(defvar *requested-native-values* nil)

(deflapfunction cvm-get-kernel-global (name)
  (check-type name ccl-symvector)
  (pushnew name *requested-native-values* :test #'uvector-equal)
  (warn "Trying to get native value of ~s" name)
  37)

(defparameter *fake-heap-image-name* nil)
(defparameter *fake-argv* (cffi:foreign-alloc :pointer :count 0 :null-terminated-p t))

(deflapfunction cvm-get-kernel-global-ptr (name dest)
  (check-type name ccl-symvector)
  (pushnew name *requested-native-values* :test #'uvector-equal)
  (check-type dest ccl-macptr)
  (setf (%macptr-value dest)
        (cond ((eq name (ccl'image-name))
               (or *fake-heap-image-name*
                   (setq *fake-heap-image-name*
                         (cffi:foreign-string-alloc 
                          (namestring *CCL-DIRECTORY*)))))
              ((eq name (ccl'argv)) *fake-argv*) ;;; *** TODO
              (t (warn "Trying to get native value of ~s (into ptr)" name)
                 37)))
  dest)

(deflapfunction cvm-foreign-bit-size (rec-spec)
  (labels ((native (obj)
             (etypecase obj
               (null nil)
               (cons (cons (native (car obj)) (native (cdr obj))))
               (fixnum obj)
               (ccl-symvector (sym-keyword obj)))))
    #+ccl (ccl::%foreign-type-or-record-size (native (car rec-spec))
                                             :bits
                                             (native (cdr rec-spec)))
    #-ccl (error "Don't know how to get record size of ~s" (native rec-spec))))


;;; ** REcord the records/data structures we need and look the up.
;; TODO arrange for the symbol manipulation at compile-time...


#+ccl
(defun record-field-spec  (path)
  (cond ((ccl-symvector-p path) (sym-keyword path))
        (t
         (assert (consp (cdr path)))
         (let* ((strings (loop for sym in path as first = t then nil
                           do (assert (eq (sym-pkg sym) *keyword-pkg*))
                           unless first collect "."
                           collect (sym-native-pname sym)))
                (name (apply #'concatenate 'string strings)))
           (intern name :keyword)))))

(deflapfunction cvm-access-foreign-field (ccl-ptr path bit-offset)
  (cassert (eql 0 bit-offset))
  (let* ((ptr (%macptr-ptr ccl-ptr))
         (spec (record-field-spec path)))
    #-ccl (error "Don't know how to access foreign field ~s" spec)
    #+ccl (ccl (eval `(ccl:pref ',ptr ,spec)))))


(deflapfunction setf-cvm-access-foreign-field (ccl-ptr path bit-offset value)
  (cassert (eql 0 bit-offset))
  (when (null value) (error "BUG: how is value null?"))
  (let* ((ptr (%macptr-ptr ccl-ptr))
         (spec (record-field-spec path)))
    #-ccl (error "Don't know how to set foreign field ~s" spec)
    #+ccl (eval `(setf (ccl:pref ',ptr ,spec) ',value))))


(defconstant u32-mask #xFFFFFFFF)

(declaim (inline u32-sign-extend))
(defun u32-sign-extend (word)
  ;(declare (type (unsigned-byte 32) word))
  ;;; *** TODO: REMOVE
  (check-type word (unsigned-byte 32))
  (if (logbitp 31 word) (logior word (ash -1 -32)) word))

;;; TODO: use this package for all the ffi defs that should come from groveling.
(defpackage "CFFI-CONSTANTS" (:use))
;(defvar *CFFI-CONSTANTS* (make-package "CFFI-CONSTANTS" :use nil))
(defconstant CFFI-CONSTANTS::RTLD_GLOBAL 8)
(defconstant CFFI-CONSTANTS::RTLD_NOLOAD 16)
(defconstant CFFI-CONSTANTS::HOST_BASIC_INFO_COUNT 12)
(defconstant CFFI-CONSTANTS::KERN_SUCCESS 0)
(defconstant CFFI-CONSTANTS::HOST_BASIC_INFO 1)
(defconstant CFFI-CONSTANTS::_SC_CLK_TCK 3)
(defconstant CFFI-CONSTANTS::PATH_MAX 1024)
(defconstant CFFI-CONSTANTS::S_IFMT  #xF000)
(defconstant CFFI-CONSTANTS::S_IFDIR #x4000)
(defconstant CFFI-CONSTANTS::S_IFREG #x8000)
(defconstant CFFI-CONSTANTS::S_IFLNK #xA000)
(defconstant CFFI-CONSTANTS::S_IFIFO #x1000)

(deflapfunction cvm-os-constant (symvec)
  (check-type symvec ccl-symvector)
  (let* ((str (sym-native-pname symvec))
         (sym (intern str :cffi-constants)))
    (unless (boundp sym)
      #-ccl (error "Don't know how to get OS constant ~s" sym)
      #+ccl (let ((val (ccl::load-os-constant sym)))
              (FORMAT T "~&;;;   CVM-OS-CONSTANT had to look up ~s [#x~x]" sym val)
              ;; load-os-constants defines the constant
              (assert (eq val (symbol-value sym)))))
    (symbol-value sym)))


;;; Predefine some foreign fns we call during startup, figure out dynamic stuff later
(cffi:defctype size_t :unsigned-int)
(cffi:defctype host_t :unsigned-int)

;;;; Very temporary, I hope..  
(defparameter *known-c-functions-alist* nil)

(defmacro def-external-call ((lisp-name string) return-type &rest argspecs)
  (let ((body-form `(,lisp-name ,@(mapcar (lambda (argspec)
                                            (destructuring-bind (var type) argspec
                                              (if (eq type :pointer)
                                                `(%macptr-ptr ,var)
                                                var)))
                                          argspecs))))
    (when (eq return-type :pointer)
      (setq body-form `(make-ccl-macptr ,body-form)))
    `(progn
       (cffi:defcfun (,lisp-name ,string)  ,return-type ,@argspecs)
       (push (cons ,string (named-function ,lisp-name ,(mapcar #'car argspecs) ,body-form))
             *known-c-functions-alist*)
       ',lisp-name)))


(def-external-call (cvmdarwin-ffi/memset "memset") :pointer
  (ptr :pointer)
  (val :int)
  (size size_t))

(def-external-call (cvmdarwin-ffi/getdtablesize "getdtablesize") :int)

(def-external-call (cvmdarwin-ffi/mach_host_self "mach_host_self") host_t)

(def-external-call (cvmdarwin-ffi/host_info "host_info") :int
  (host host_t)
  (flavor :int)
  (host-info-out :pointer)
  (host-info-out-cnt :pointer))

(def-external-call (cvmdarwin-ffi/getpagesize "getpagesize") :int)

(def-external-call (cvmdarwin-ffi/sysconf "sysconf") :long
  (name :int))

(def-external-call (cvmdarwin-ffi/getuid "getuid") :int)

(def-external-call (cvmdarwin-ffi/getenv "getenv") :pointer
  (name :pointer))

(def-external-call (cvmdarwin-ffi/getpwuid_r "getpwuid_r") :int
  (uid :int)
  (pwd :pointer)
  (buffer :pointer)
  (size size_t)
  (result :pointer))

(defun get-external-fn (sym)
  (let ((name (sym-native-pname sym)))
    ;; If this was more permanent --- instead of using name string, map from the SYM, which is in the cvmdarwin-ffi package.
    (or (cdr (assoc name *known-c-functions-alist* :test 'equal))
        (progn
          (cerror "try again" "unknown external fn ~s (~s)" sym (cffi:foreign-symbol-pointer name))
          (get-external-fn sym)))))

(deflapfunction cvm-external-call (sym &rest args)
  (assert (eq (sym-pkg sym) *ffi-pkg*))
  ;; ok, so this depends on us KNOWING the arg/value convention of the fn
  (let ((ffn (sym-fboundp sym)))
    (unless ffn
      (setf (sym-func sym) (setq ffn (get-external-fn sym))))
    (apply ffn args)))


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
    
(deflapfunction %ilogcount (number) (logcount number))


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
  (let ((vec (uvector-data bignum)))
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
      (setf (uvector-data bignum) (vector (logand fixnum u32-mask)))
      (let ((vec (uvector-data bignum)))
        (setf (svref vec 0) low)
        (setf (svref vec 1) (logand (ash high1 -1) u32-mask)))))
  fixnum)

(deflapfunction %fixnum-intlen (number) (integer-length (the fixnum number)))

(deflapfunction %set-bignum-length (newlen bignum)
  (let* ((vec (uvector-data bignum))
         (oldlen (length vec)))
    (assert (<= newlen oldlen))
    (unless (eql newlen oldlen)
      (setf (uvector-data bignum) (subseq vec 0 newlen)))))
  
(deflapfunction %bignum-hash (bignum)
  (let* ((vec (if (typep bignum 'simple-vector) ;;** for testing only
                 bignum
                 (uvector-data bignum)))
         (len (length vec))
         (hash (+ (ash len 8) subtag-bignum)))
    ;; So at all times, hash is 32 bits because addl clears high word!!!  I think taht rolq should be roll !!
    #+OLD (loop for digit across vec
            do (setq hash (logior (ash hash -51)
                                  (ash (logand hash (1- (ash 1 51))) 13)))
            do (setq hash (logand #xFFFFFFFF (+ hash digit))))
    (loop for digit across vec
      do (setq hash (logand #xFFFFFFFF (+ digit (ash hash 13)))))
    #+ccl(unless (eql hash (ccl::%bignum-hash (native-integer bignum)))
           (break "mismatched hash for ~s: us ~s ccl ~s" bignum hash(ccl::%bignum-hash (native-integer bignum))))
    hash))

(deflapfunction fix-digit-logand (fix big dest)
  (let ((res (logand fix (svref (uvector-data big) 0))))
    (if (null dest)
      res
      (progn
        (setf (svref (uvector-data dest) 0) res)
        dest))))

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
      (let* ((vec (uvector-data ccl-number))
             (end (1- (length vec)))
             (bignum (if (logbitp 31 (svref vec end)) -1 0)))
        (loop for i from end downto 0
          do (setq bignum (logior (ash bignum 32) (svref vec i))))
        bignum)
      (error "Not an integer ~s" ccl-number))))


(deflapfunction %get-gc-count () 17)

;; This gets big, because there is an eq hash table of functions to lfun names.
;;; TODO THIS NEEDS TO BE WEAK  Check weak support in sbcl/lispworks
;;; This is basically a big hash table of all the CCL objects that are ever
;;; stored in an EQ hash table, heh.
(defparameter *fake-addresses-table* (make-hash-table :test 'eq))

;; for instance hash, the address is just used as an initial hash, but
;; must not conflict with max-class-ordinal
(defconstant max-class-ordinal (ash 1 20))

;;; *** TODO: anything where we check the subtag for a specific thing and we check if it's a uvector first,
;;;  make it a ccl-uvector substruct and check the type instead.


(deflapfunction strip-tag-to-fixnum (obj)
  (cond ((typep obj 'fixnum) obj)
        ((characterp obj) (char-code obj))
        ((typep obj 'single-float)
         (multiple-value-bind (m exp sign) (integer-decode-float obj)
           (let ((uexp (+ exp (ash 1 12))))
             ;; Just put them tegether in any consistent way
             (check-type m (unsigned-byte 32))
             (check-type uexp (unsigned-byte 12))
             (logior m
                     (ash uexp 32)
                     (if (eql sign -1) (ash 1 (+ 32 12)) 0)))))
        ;; *** ACTUALLY I THIINK THIS IS ONLY SUPPOSED TO HAPPEN FOR FOREIGNN CLASSES?
        ((and (ccl-uvector-p obj) (eq (uvector-subtag obj) subtag-instance))
         (+ (1+ max-class-ordinal) (random (- most-positive-fixnum (1+ max-class-ordinal)))))
        (t (or (gethash obj *fake-addresses-table*)
               (setf (gethash obj *fake-addresses-table*)
                     (1+ (hash-table-count *fake-addresses-table*)))))))

;; ccl has fast-mod, why doesn't it have an optimizer to use it??
;; sbcl does use this.
(deflapfunction fast-mod (num divisor)
  ;;(declare (type (unsigned-byte #.(1- num-fixnum-bits)) num divisor))
  ;;; TODO: remove
  ;(check-type num (unsigned-byte #.(1- num-fixnum-bits)))
  ;(check-type divisor (unsigned-byte #.(1- num-fixnum-bits)))
  (mod num divisor))


(deflapfunction fast-mod-3 (number divisor recip)
  (unless (and (fixnump number) (fixnump divisor) (fixnump recip))
    (error "not fixnums ~s[~s] ~s[~s] ~s[~s]"
           number (fixnump number) divisor (fixnump divisor) recip (fixnump recip)))
  (let* ((res
          (let* ((number (if (< number 0) (logand number #x1FFFFFFFFFFFFFFF) number));; *** there's probably a better way
                 (high (ash (* number recip) -61))
                 (low (logand (1- (ash 1 61)) (* high divisor)))
                 (result (- number low divisor)))
            (if (logbitp 60 result)
              (+ result divisor)
              result))))
    ;;; TODO: remove
    ;; ash shift LEFT
    ;; Values as they appear in registers
    (let ((cres (ccl::fast-mod-3 number divisor recip)))
      (unless (eq res cres)
        (break "fast-mod-3 ~s ~s ~s our ~s ccl ~s"
               number divisor recip res (if (fixnump cres) cres (list 'bogus (ccl::strip-tag-to-fixnum cres))))))
    res))

(defun kernel-import-malloc (size)
  (make-ccl-macptr (cffi:foreign-alloc :int8 :count size)))

(defun kernel-import-fd-setsize-bytes () ;;;; **** TODO 
  ;; sizeof(fd_set)
  128)

(defun kernel-import-new-recursive-lock ()
  (make-ccl-macptr (cffi:foreign-alloc :int8 :count lockptr.size :initial-element 0)))

(defun kernel-import-rwlock-new ()
  (make-ccl-macptr (cffi:foreign-alloc :int8 :count rwlock.size :initial-element 0)))

(defun kernel-import-new-semaphore (n)
  (declare (ignore n))
  (make-ccl-macptr 13))

(defun kernel-import-wait-on-semaphore (address seconds millis)
  (declare (ignore address seconds millis))
  0)

(defun kernel-import-signal-semaphore (semaphore)
  (declare (ignore semaphore))
  0)

(defun kernel-import-wait-for-signal (signo seconds millis)
  (declare (ignore signo seconds millis))
  0)

(cffi:defcfun (ff-gettimeofday "gettimeofday") :int
  (ptimeval :pointer)
  (ptz :pointer))

(cffi:defcfun (ff-realpath "realpath") :pointer
  (file_name  :pointer)
  (result :pointer))

(cffi:defcfun (ff-stat "stat$INODE64") :int
  (file_name  :pointer)
  (buf :pointer))

(cffi:defcfun (ff-fstat "fstat$INODE64") :int
  (fd  :int)
  (buf :pointer))

;; Wonder why these in the kernel
(defun kernel-import-lisp-gettimeofday (ptimeval ptz)
  (ff-gettimeofday (%macptr-ptr ptimeval) (%macptr-ptr ptz)))

(defun kernel-import-lisp-realpath (nameptr resultptr)
  (make-ccl-macptr
   (ff-realpath (%macptr-ptr nameptr) (%macptr-ptr resultptr))))

(defun kernel-import-lisp-stat (nameptr statptr)
  (assert (not (eql 0 (%macptr-value statptr)))) ;; for debuggging
  (ff-stat (%macptr-ptr nameptr) (%macptr-ptr statptr)))

(defun kernel-import-lisp-fstat (fd statptr)
  (ff-fstat fd (%macptr-ptr statptr)))

(deflapfunction %get-spin-lock (spin) spin)
(deflapfunction %lock-gc-lock () 0)
(deflapfunction %unlock-gc-lock () 0)

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

(let ((lock (make-rw-lock-obj)))
  (setf (sym-value (ccl '%all-packages-lock%)) lock)
  (setf (sym-value (ccl '%system-locks%)) (make-uvector subtag-population (vector 0 0 (svref (uvector-data lock) 0)))))


(defconstant node-size 8)

;; No threads, no big deal!  Except we have to reverse-engineer the offset
;; offset = 8*(index+1) - tag
(defun uvector-offset-to-cell-index (uvec offset)
  (cassert (gvector-type-p (uvector-subtag uvec)))
  (let* ((tag (ecase (logand offset 7)
                (#.(logand (- fulltag-misc) 7) fulltag-misc)
                (#.(logand (- fulltag-symbol) 7) fulltag-symbol)))
         (index (1- (ash (+ offset tag) -3))))
    (assert (< -1 index (length (uvector-data uvec))))
    index))

(deflapfunction %store-node-conditional (node-offset object old new)
  (etypecase object
    (ccl-uvector (let ((index (uvector-offset-to-cell-index object node-offset))
                       (vec (uvector-data object)))
                   (when (eq old (svref vec index))
                     (setf (svref vec index) new)
                     T)))))


(deflapfunction %set-hash-table-vector-key-conditional (node-offset vector old new)
  (let ((vec (uvector-data vector))
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
                   (incf (svref (uvector-data object) index) by)))))


  


(deflapfunction closure-function (func) (ccl-closure-function func))

(deflapfunction %symptr->symbol (symvector)
  (if (eq symvector *nil-sym*) nil
    (if (eq symvector *t-sym*) t
      (require-type symvector 'ccl-symvector))))



(deflapfunction %symptr-value %symptr-value)
(deflapfunction %set-symptr-value %set-symptr-value)

(deflapfunction %set-hash-table-vector-key (vector index value)
  (setf (svref (uvector-data vector) index) value))


(deflapfunction %string-hash (start str len)
  (check-type str ccl-simple-base-string)
  (loop with vec = (uvector-data str)
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
           (let ((subtag (uvector-subtag x)))
             (and (eq subtag (uvector-subtag y))
                  (cond ((eq subtag subtag-macptr)
                         (eql (%macptr-value x) (%macptr-value y)))
                        ((logbitp subtag numeric-subtag-mask)
                         (let ((xv (uvector-data x))
                               (yv (uvector-data y)))
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
             (let ((xv (uvector-data x))
                   (yv (uvector-data y)))
               (and (eql (length xv) (length yv)) (every #'eql xv yv))))
            (t
             (let ((tag (fulltag x)))
               (and (eq tag (Fulltag y))
                    (cond ((eq tag fulltag-misc)
                           (ccl-funcall (ccl 'hairy-equal) x y))
                          (t nil))))))))

(deflapfunction %type-of (x)
  (let ((type (%type-name-of x)))
    (if (eq type 'lock)
      (svref (uvector-data x) 1)
      (ccl-symbol type))))

(deflapfunction true (&rest ignore)
  (declare (ignore ignore))
  t)

(deflapfunction false (&rest ignore)
  (declare (ignore ignore))
  nil)

(deflapfunction host-single-float-from-unsigned-byte-32 (u32)
  ;; Do what ccl would do to convert these into standard integer-decode-float values
  ;; then can re-encode them in the host lisp
  (let ((mantissa (ldb (byte 23 0) u32))
        (exp (- (ldb (byte 8 23) u32) 150)))
    (assert (not (eq exp 105))) ;; nan/inf
    (if (eql exp -150)
      (unless (eql 0 mantissa)
        (loop
          (setq mantissa (ash mantissa 1))
          (when (logbitp 23 mantissa) (return))
          (setq exp (1- exp))))
      (setq mantissa (logior (ash 1 23) mantissa)))
    (let ((float (scale-float (float mantissa 1.0s0) exp)))
      (if (logbitp 31 u32) (- float) float))))

;; 1 bit sign, 11 bits exp 52 bits mantissa, but it's treated as 2 32-bit values
;; This duplicates x8664.  Do we dare have a totally different rep?  Does ccl
;; ever dig into the floats outside the backend? [Yes: %copy-float for macptr to float,
;; simple-1d-array-subseq]
(deflapfunction %make-float-from-fixnums (dfloat hi low exp sign)
  (check-type exp (unsigned-byte 11))
  ;; (check-type hi (unsigned-byte 24)) ;; nope, passes in  #x1ffffff
  (setq hi (logand (1- (ash 1 24)) hi))
  (check-type low (unsigned-byte 28))
  (check-type sign (member 1 0 -1)) ;; 1 and 0 are the same...
  (let* ((vec (uvector-data dfloat))
         (loword (logior (ash (logand hi #xF) 28) low))
         (hiword (ash hi -4)))
    (assert (eql (uvector-subtag dfloat) subtag-double-float))
    (setf (svref vec 0) loword)
    (setf (svref vec 1) (logior (logand sign (ash 1 31))
                                (ash exp 20)
                                hiword))))

;; (ccl::add-bignum-and-fixnum  #(0 0 1) -1)  #(0 0 1) is (ash 1 64)

(defun native-double-float (dfloat)
  (let* ((vec (uvector-data dfloat))
         (loword (svref vec 0))
         (hiword (svref vec 1))
         (mantissa (logior (ash (ldb (byte 20 0) hiword) 32) loword))
         (exp (- (ldb (byte 11 20) hiword) 1074)))
    (unless (zerop exp)
      (setq mantissa (logior mantissa (ash 1 52)))
      (setq exp (1- exp)))
    (let ((float (scale-float (float mantissa 1.0d0) exp)))
      (if (logbitp 31 hiword) (- float) float))))

;;; stuff that was in nfasload
(deflapfunction register-package-ref (name)
  (register-package-ref name (pkg-arg name nil)))

(deflapfunction pkg-arg (thing &optional deleted-ok (errorp t))
  (if (and deleted-ok (ccl-package-p thing))
    thing
    (pkg-arg thing errorp)))

(deflapfunction find-package (thing)
  (if (ccl-package-p thing)
    thing
    (pkg-arg thing nil)))

(deflapfunction %new-package-hashtable (size)
  (make-hash-table :test 'equal :size size))


(deflapfunction %find-symbol (string len package)
  (check-type string ccl-simple-base-string)
  (assert (eq len (length (uvector-data string))))
  (multiple-value-bind (sym where) (find-sym-in-pkg string package)
    (values sym (ccl-symbol where) -23 -17)))

(deflapfunction  %insert-symbol (symbol package i e)
  (assert (and (eq i -23) (eq e -17))) ;; make sure it's coming straight from %find-symbol
  (add-sym-to-pkg symbol package))

(deflapfunction %add-symbol (pname pkg internal-idx external-idx &optional force-export)
  (when force-export (error "FORCE-EXPORT not implemented yet"))
  (assert (and (eql internal-idx -23) (eql external-idx -17)))
  (add-sym-to-pkg (make-ccl-symvector pname) pkg))


(deflapfunction %export-symbol (sym package)
  (export-sym-from-pkg (sym-symvector sym) package)
  t)

(deflapfunction provide (module) ;; bootstrapping version
  (when (ccl-symvector-p module) (setq module  (sym-pname module)))
  (check-type module ccl-simple-base-string)
  (pushnew module (sym-value (ccl'*modules*)) :test 'uvector-equal))


(deflapfunction %class-of-instance (instance)
  (svref (uvector-data (svref (uvector-data instance) instance.class-wrapper)) %wrapper.class))

(deflapfunction class-of (object)
  (let ((info (svref (uvector-data (sym-value (ccl '*class-table*)))
                     (if (ccl-uvector-p object)
                       (uvector-subtag object)
                       (fulltag object)))))
    (cond ((null info)
           (error "Don't know the class of ~s" object))
          ((ccl-function-p info)
           (ccl-funcall info object))
          (t info))))

(deflapfunction %function (sym)
   ;; err on  macros/special forms
  (require-type (sym-func sym) 'ccl-function))

(deflapfunction %init-misc (val uvector)
  (if (ccl-simple-base-string-p uvector)
    (unless (characterp val) (setq val (code-char val))))
  (let ((vec (uvector-data uvector)))
    (loop for i from 0 below (length vec) do (setf (svref vec i) val))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Generic functions

(defmacro def-gf-proto (name args-or-lap-name &body body)
  `(let* ((sym (ccl-symbol ',name))
          (fn (%make-ccl-function :subtag subtag-function
                                  :data (vector sym 0)
                                  :bslambda 'gf-proto
                                  :native-fn ,(if (listp args-or-lap-name)
                                                `(named-function ,name ,args-or-lap-name ,@body)
                                                `(function ,args-or-lap-name)))))
     (setf (sym-func sym) fn)))

(def-gf-proto gag-any-arg (env self args)
  (let ((dt (svref (uvector-data self) 2))
        (dcode (svref (uvector-data self) 3)))
    (apply-in-environment env dcode (list dt args))))

(%defvar (ccl '*gf-proto*) nil 'variable (sym-func (ccl 'gag-any-arg)))

(def-gf-proto gag-one-arg (env self args)
  (assert (eql (length args) 1))
  (let ((dt (svref (uvector-data self) 2))
        (dcode (svref (uvector-data self) 3)))
    (apply-in-environment env dcode (list* dt args))))

(def-gf-proto gag-two-arg (env self args)
  (assert (eql (length args) 2))
  (let ((dt (svref (uvector-data self) 2))
        (dcode (svref (uvector-data self) 3)))
    (apply-in-environment env dcode (list* dt args))))

(def-gf-proto funcallable-trampoline (env self args)
  (let ((dcode (svref (uvector-data self) 3)))
    (apply-in-environment env dcode args)))

(def-gf-proto unset-fin-trampoline (env self args)
  (signal-error $xnofinfunction self args env))

(deflapfunction replace-function-code (target proto)
  (assert (eq (ccl-function-bslambda proto) 'gf-proto))
  (setf (ccl-function-native-fn target) (ccl-function-native-fn proto)))

(defun make-cloned-fn (type data native-fn)
  (check-type type symbol)
  (%make-ccl-function :subtag subtag-function
                      :bslambda type
                      :data data
                      :native-fn native-fn))

(deflapfunction cvm-make-gf (proto &rest data)
  (assert (eq (ccl-function-bslambda proto) 'gf-proto))
  ;;; *** REMOVE THIS ONCE DEBUGGED -- add a name so we  know where it comes from
  ;; (assert (not (logbitp $lfbits-noname-bit (car (last data))))) ;; not always true, sigh
  (unless (logbitp $lfbits-noname-bit (car (last data)))
    (setq data (append (butlast data)
                       (list (if (eq proto (sym-func (ccl'unset-fin-trampoline)))
                               (ccl 'consed-gf)
                               (ccl 'early-consed-gf)))
                       (last data))))
  (make-cloned-fn 'gf (coerce data 'vector) (ccl-function-native-fn proto)))

(deflapfunction cvm-make-combined-method (thing dcode gf-or-cm bits)
  ;; (assert (logbitp $lfbits-noname-bit bits)) ;; or is the GF the name?
  (make-cloned-fn 'combined-method
                  (vector thing dcode gf-or-cm bits)
                  (named-function combined-method-code (env self args)
                    (let* ((data (uvector-data self))
                           (thing (svref data 0))
                           (dcode (svref data 1)))
                      (funcall-in-environment env dcode thing args)))))

(deflapfunction cvm-make-reader-method (slot-id lookup name bits)
  (make-cloned-fn 'reader-method
                  (vector slot-id lookup name bits)
                  (named-function reader-method-code (env self args)
                    (destructuring-bind (instance) args
                      (let* ((data (uvector-data self))
                             (slot-id (svref data 0)))
                        (funcall-in-environment env (svref data 1) instance slot-id))))))


(deflapfunction cvm-make-writer-method (slot-id lookup name bits)
  (make-cloned-fn 'writer-method
                  (vector slot-id lookup name bits)
                  (named-function cvm-writer-method-code (env self args)
                    (destructuring-bind (new instance) args
                      (let* ((data (uvector-data self))
                             (slot-id (svref data 0)))
                        (funcall-in-environment env (svref data 1)
                                                instance slot-id new))))))


(deflapfunction cvm-make-slot-lookup-fn (map table bits)
  (make-cloned-fn 'slot-lookup
                  (vector map table bits)
                  (named-function cvm-slot-lookup-fn-code (env self args)
                    (declare (ignore env))
                    (destructuring-bind (slot-id) args
                      (let* ((data (uvector-data self))
                             (map-data (svref data 0))
                             (table (svref data 1))
                             (index (uvref slot-id slot-id.index)))
                        (uvref table
                               (if (< index (length map-data)) (svref map-data index) 0)))))))

(deflapfunction cvm-make-slot-getter (map table class lookup missing bits)
  (make-cloned-fn 'slot-getter
                  (vector map table class lookup missing bits)
                  (named-function cvm-slot-getter-code (env self args)
                    (destructuring-bind (instance slot-id) args
                      (let* ((data (uvector-data self))
                             (map-data (uvector-data (svref data 0)))
                             (table (svref data 1))
                             (index (uvref slot-id slot-id.index)))
                        (if (or (>= index (length map-data))
                                (eql 0 (setq index (svref map-data index))))
                          (funcall-in-environment env (svref data 4)  ;; missing
                                                  instance slot-id)
                          (funcall-in-environment env (svref data 3) ;; lookup using class
                                                  (svref data 2) instance (uvref table index))))))))

(deflapfunction cvm-make-slot-setter (map table class lookup missing bits)
  (make-cloned-fn 'slot-setter
                  (vector map table class lookup missing bits)
                  (named-function cvm-slot-setter-code (env self args)
                    (destructuring-bind (instance slot-id val) args
                      (let* ((data (uvector-data self))
                             (map-data (uvector-data (svref data 0)))
                             (table (svref data 1))
                             (index (uvref slot-id slot-id.index)))
                        (if (or (>= index (length map-data))
                                (eql 0 (setq index (svref map-data index))))
                          (funcall-in-environment env (svref data 4)  ;; missing
                                                  instance slot-id val)
                          (funcall-in-environment env (svref data 3) ;; set using class
                                                  (svref data 2) instance (uvref table index) val)))))))


(deflapfunction %apply-with-method-context (magic func args &environment env)
  (check-type func ccl-function)
  (let ((bits (ccl-function-bits func)))
    (assert (logbitp $lfbits-method-bit bits))
    (when (logbitp $lfbits-nextmeth-bit bits)
      (push magic args)))
  ;; -KNOWN-METHOD thing is just for typechecking
  (apply-in-environment-KNOWN-METHOD env func args))

;; Seriously?? It can't just use a closure?  Maybe the FN thing is used somewhere?
(deflapfunction cvm-make-type-fn (datum fn name bits)
  (make-cloned-fn 'type-predicate
                  (vector datum fn name bits)
                  (named-function type-fn-code (env self args)
                    (destructuring-bind (thing) args
                      (funcall-in-environment env (ccl'%%typep) thing (uvref self 0))))))




(deflapfunction %nth-immediate (fn index)
  (check-type fn ccl-function)
  (svref (uvector-data fn) index))

(deflapfunction %set-nth-immediate (fn index value)
  (check-type fn ccl-function)
  (setf (svref (uvector-data fn) index) value))