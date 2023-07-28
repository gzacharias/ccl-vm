(in-package :ccl-vm)

;;; TODO: ** Are all args to CCL quoted?  Maybe it should be a macro

;;; package for FFI defs that should come from groveling
(defpackage "CCL-FFI" (:use))


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
  (cond ((eq name (ccl'batch-flag))    0) ;; don't want batch mode
        (t 37)))

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
                          ;; This is used only to set the CCL: logical name. It must be a file that exists,
                          ;; inside the ccl directory (if we just use the directory, last component gets stripped)
                          (namestring (make-pathname :name "cvmsrcs" :defaults *CCL-DIRECTORY*))))))
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
  (if (logbitp 31 word) (logior word (ash -1 32)) word))

;;; Predefine some foreign fns we call during startup, figure out dynamic stuff later
(cffi:defctype ccl-ffi::host_t :unsigned-int)
(cffi:defctype ccl-ffi::size_t :uint64)
(cffi:defctype ccl-ffi::offset_t :int64)


(defconstant CCL-FFI::RTLD_GLOBAL 8)
(defconstant CCL-FFI::RTLD_NOLOAD 16)
(defconstant CCL-FFI::HOST_BASIC_INFO_COUNT 12)
(defconstant CCL-FFI::KERN_SUCCESS 0)
(defconstant CCL-FFI::HOST_BASIC_INFO 1)
(defconstant CCL-FFI::_SC_CLK_TCK 3)
(defconstant CCL-FFI::PATH_MAX 1024)
(defconstant CCL-FFI::S_IFMT  #xF000)
(defconstant CCL-FFI::S_IFDIR #x4000)
(defconstant CCL-FFI::S_IFREG #x8000)
(defconstant CCL-FFI::S_IFLNK #xA000)
(defconstant CCL-FFI::S_IFIFO #x1000)
(defconstant CCL-FFI::SEEK_CUR 1)
(defconstant CCL-FFI::O_RDWR 2)
(defconstant CCL-FFI::ENOENT 2)
(defconstant CCL-FFI::ENFILE #x17)
(defconstant CCL-FFI::EMFILE #x18)
(defconstant CCL-FFI::_PC_MAX_INPUT 3)


(deflapfunction cvm-os-constant (symvec)
  (check-type symvec ccl-symvector)
  (let* ((str (sym-native-pname symvec))
         (sym (intern str :CCL-FFI)))
    (unless (boundp sym)
      #-ccl (error "Don't know how to get OS constant ~s" sym)
      #+ccl (let ((val (ccl::load-os-constant sym)))
              (FORMAT T "~&;;;   CVM-OS-CONSTANT had to look up ~s [#x~x]" sym val)
              ;; load-os-constants defines the constant
              (assert (eq val (symbol-value sym)))))
    (symbol-value sym)))




;;;; Very temporary, I hope..  
(defparameter *known-c-functions-alist* nil)

(defmacro def-external-call (name-spec return-type &rest argspecs)
  (when (stringp name-spec)
    (setq name-spec (list (intern (string-upcase name-spec) :CCL-FFI) name-spec)))
  (destructuring-bind (lisp-name string) name-spec
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
         (push (cons ,string
                     (named-function ,lisp-name ,(mapcar #'car argspecs) ,body-form))
               *known-c-functions-alist*)
         ',lisp-name))))


(def-external-call "memset" :pointer
  (ptr :pointer)
  (val :int)
  (size ccl-ffi::size_t))

(def-external-call "getdtablesize" :int)

(def-external-call "mach_host_self" ccl-ffi::host_t)

(def-external-call "host_info" :int
  (host ccl-ffi::host_t)
  (flavor :int)
  (host-info-out :pointer)
  (host-info-out-cnt :pointer))

(def-external-call "getpagesize" :int)

(def-external-call "sysconf" :long
  (name :int))

(def-external-call "getuid" :int)

(def-external-call "getenv" :pointer
  (name :pointer))

(def-external-call "getpwuid_r" :int
  (uid :int)
  (pwd :pointer)
  (buffer :pointer)
  (size ccl-ffi::size_t)
  (result :pointer))

(def-external-call "isatty" :int
  (fd :int))

(def-external-call "fpathconf" :long
  (fd :int)
  (size :int))

(defun get-external-fn (sym)
  (let ((name (sym-native-pname sym)))
    (or (cdr (assoc name *known-c-functions-alist* :test 'equal))
        (progn
          (cerror "try again" "unknown external fn ~s (~s)" sym (cffi:foreign-symbol-pointer name))
          (get-external-fn sym)))))

;; TODO: Could init all the functions first time this is called, then set *known-c-functions-alist* to nil
(deflapfunction cvm-external-call (sym &rest args)
  (assert (eq (sym-pkg sym) *ffi-pkg*))
  (let ((ffn (sym-fboundp sym)))
    (unless ffn
      (setf (sym-func sym)
            (setq ffn (make-cloned-fn 'lap
                                      (vector sym (dpb (length args) $lfbits-numreq 0))
                                      (get-external-fn sym)))))
    (ccl-apply ffn args)))


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
  (case (uvsize bignum)
    (1 (u32-sign-extend (uvref bignum 0)))
    (2 (let* ((low (uvref bignum 0))
              (high (uvref bignum 1))
              (too-high (ash high -28))) ;; sign bit + extra
         (when (or (eql too-high 0) (eql too-high #xF))
           (logior (ash (u32-sign-extend high) 32) low))))
    (t nil)))

(deflapfunction %truncate-short-float->fixnum (f) (truncate f))

(deflapfunction %fixnum-to-bignum-set (bignum fixnum)
  ;; bignum is a two-digit bignum
;;; The caller has allocated a two-digit bignum (quite likely on the stack).
;;; If we can fit in a single digit (if the high word is just a sign
;;; extension of the low word), truncate the bignum in place (the
;;; trailing words should already be zeroed.
  (let ((high1 (ash fixnum -31))
        (low (logand fixnum u32-mask)))
    (setf (uvref bignum 0) low)
    (if (or (eql high1 0) (eql high1 -1))
      (lap-%set-bignum-length 1 bignum)
      (setf (uvref bignum 1) (logand (ash high1 -1) u32-mask))))
  fixnum)

(deflapfunction %fixnum-intlen (number) (integer-length (the fixnum number)))

(deflapfunction %set-bignum-length (newlen bignum)
  (let ((oldlen (uvsize bignum)))
    (assert (<= newlen oldlen))
    (unless (eql newlen oldlen)
      (with-uvector-data (vec bignum)
        (error "Can't %set-bignum-length of heap vector ~s" bignum)
        (setf (uvector-data bignum) (subseq vec 0 newlen))))))
  
(deflapfunction %bignum-hash (bignum)
  (let* ((len (uvsize bignum))
         (hash (+ (ash len 8) subtag-bignum)))
    (with-uvector-data (vec bignum)
      (setq hash (error "Should implement ~s" `(heap-vector-bignum-hash ,bignum)))
      ;; So at all times, hash is 32 bits because addl clears high word!!!  I think that rolq should be roll !!
      ;; TODO: report this ^^^ (try it out)
      #+OLD (loop for digit across vec
              do (setq hash (logior (ash hash -51)
                                    (ash (logand hash (1- (ash 1 51))) 13)))
              do (setq hash (logand #xFFFFFFFF (+ hash digit))))
      (loop for digit across vec
        do (setq hash (logand #xFFFFFFFF (+ digit (ash hash 13))))))
    #+ccl (let ((native (ccl::%bignum-hash (native-integer bignum))))
            (unless (eql hash native)
              (break "mismatched hash for ~s: us ~s ccl ~s" bignum hash native)))
    hash))

(deflapfunction fix-digit-logand (fix big dest)
  (let ((res (logand fix (uvref big 0))))
    (if (null dest)
      res
      (progn
        (setf (uvref dest 0) res)
        dest))))

(deflapfunction %multiply-and-add-fixnum-loop (len64 bignum fixnum result)
  (declare (ignore len64))
  (check-type fixnum fixnum)
  (with-uvector-data (resultv result)
    (error "Heap vector bignums not supported")
    (let* ((val (native-integer bignum))
           (res (* val fixnum)))
      (data-for-bignum res resultv)
      result)))

(defun multiply-and-add-loop (bignum mult result)
  (with-uvector-data (resultv result)
    (error "Heap vector bignums not supported")
    (let* ((val (native-integer bignum))
           (res (* val mult)))
      (data-for-bignum res resultv)
      result)))

(deflapfunction %multiply-and-add-loop64 (x y result i len-y) ;; x[i] * y
  (declare (ignore len-y))
  (with-uvector-data (resultv result)
    (error "Heap vector bignums not supported")
    (let* ((pos (* i 2))
           (lo (REQUIRE-TYPE (uvref x pos) '(unsigned-byte 32)))
           (hi (REQUIRE-TYPE (if (< (1+ pos) (uvsize x)) (uvref x (1+ pos)) 0) '(UNSIGNED-BYTE 32)))
           (mult (+ (ash hi 32) lo))
           (res (+ (native-integer result)
                   (ash (* mult (native-integer y)) (* i 64)))))
      (data-for-bignum res resultv)
      result)))

(defun data-for-bignum (bignum &optional dest-vec)
  (let* ((bits (integer-length bignum)) ;; bits not including sign
         (size (ceiling (1+ bits) 32))
         (vec (or dest-vec (make-array size))))
    (assert (<= size (length vec)))
    (loop for i from 0 below size
      do (setf (svref vec i) (logand bignum u32-mask))
      do (setq bignum (ash bignum (- 32))))
    vec))

(defun ccl-bignum (bignum)
  (make-ccl-bignum :subtag subtag-bignum :data (data-for-bignum bignum)))

(defun native-integer (ccl-number)
  (if (fixnump ccl-number)
    ccl-number
    (if (ccl-bignum-p ccl-number)
      (let* ((end (1- (uvsize ccl-number)))
             (bignum (if (logbitp 31 (uvref ccl-number end)) -1 0)))
        (loop for i from end downto 0
          do (setq bignum (logior (ash bignum 32) (uvref ccl-number i))))
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

(cffi:defcfun (ff-lseek "lseek") ccl-ffi::offset_t
  (fildes :int)
  (offset ccl-ffi::offset_t)
  (whence :int))


(cffi:defcfun (ff-open "open") :int
  (path :pointer)
  (flag :int)
  (mode :uint16))

(defun kernel-import-lisp-lseek (fd offset whence)
  (ff-lseek fd offset whence))

(defun kernel-import-lisp-open (ptr flags mode)
  (ff-open (%macptr-ptr ptr) flags mode))


(defun kernel-import-lisp-gettimeofday (ptimeval ptz)
  (ff-gettimeofday (%macptr-ptr ptimeval) (%macptr-ptr ptz)))

(defun kernel-import-lisp-realpath (nameptr resultptr)
  (unless (cffi:null-pointer-p (%macptr-ptr resultptr))
    (setf (cffi:mem-ref (%macptr-ptr resultptr) :uint8 0) 0))
  (let ((res (ff-realpath (%macptr-ptr nameptr) (%macptr-ptr resultptr))))
    (make-ccl-macptr res)))

(defun kernel-import-lisp-stat (nameptr statptr)
  (assert (not (eql 0 (%macptr-value statptr)))) ;; for debuggging
  (ff-stat (%macptr-ptr nameptr) (%macptr-ptr statptr)))

(defun kernel-import-lisp-fstat (fd statptr)
  (ff-fstat fd (%macptr-ptr statptr)))

(cffi:defcvar ("errno" *ff-errno*) :int)

(deflapfunction %get-errno () (- *ff-errno*))

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
  (setf (sym-value (ccl '%system-locks%)) (make-uvector subtag-population (vector 0 0 (gvref lock 0)))))


(defconstant node-size 8)

;; No threads, no big deal!  Except we have to reverse-engineer the offset
;; offset = 8*(index+1) - tag
(defun uvector-offset-to-cell-index (uvec offset)
  (cassert (gvector-type-p (uvector-subtag uvec)))
  (let* ((tag (ecase (logand offset 7)
                (#.(logand (- fulltag-misc) 7) fulltag-misc)
                (#.(logand (- fulltag-symbol) 7) fulltag-symbol)))
         (index (1- (ash (+ offset tag) -3))))
    (assert (< -1 index (uvsize uvec)))
    index))

(deflapfunction %store-node-conditional (node-offset object old new)
  (etypecase object
    (ccl-uvector (let ((index (uvector-offset-to-cell-index object node-offset)))
                   (when (eq old (uvref object index))
                     (setf (uvref object index) new)
                     T)))))


(deflapfunction %set-hash-table-vector-key-conditional (node-offset vector old new)
  (lap-%store-node-conditional node-offset vector old new))


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
                   (incf (uvref object index) by)))))


(deflapfunction closure-function (func) (ccl-closure-function func))

(deflapfunction %symptr->symbol (symvector)
  (if (eq symvector *nil-sym*) nil
    (if (eq symvector *t-sym*) t
      (require-type symvector 'ccl-symvector))))



(deflapfunction %symptr-value %symptr-value)
(deflapfunction %set-symptr-value %set-symptr-value)

(deflapfunction %set-hash-table-vector-key (vector index value)
  (gvset vector index value))


(deflapfunction %string-hash (start str len)
  (check-type str ccl-simple-string)
  (with-uvector-data (vec str)
    (error "Should implement  ~s" `(heap-vector-string-hash ,str))
    (loop for hash = 0 then (logxor (logior (logand (ash hash 5) u32-mask) (ash hash -27))
                                    (char-code (aref vec index)))
      for index from start below len
      finally (return hash))))

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
                           (if (or (typep xv 'cffi:foreign-pointer)
                                   (typep yv 'cffi:foreign-pointer))
                             (error "Should implement ~s" `(heap-vector-equal ,x ,y))
                             (and (eql (length xv) (length yv))
                                  (every #'lap-eql xv yv)))))
                        (t nil)))))))


(deflapfunction equal (x y)
  (or (eq x y)
      (cond ((consp x)
             (and (consp y)
                  (lap-equal (car (the cons x)) (car (the cons y)))
                  (lap-equal (cdr (the cons x)) (cdr (the cons y)))))
            ((and (ccl-simple-string-p x) (ccl-simple-string-p y))
             (let ((xv (uvector-data x))
                   (yv (uvector-data y)))
               (if (or (typep xv 'cffi:foreign-pointer)
                       (typep yv 'cffi:foreign-pointer))
                 (error "Should implement ~s" `(heap-vector-equal ,x ,y))
                 (and (eql (length xv) (length yv)) (every #'eql xv yv)))))
            (t
             (let ((tag (fulltag x)))
               (and (eq tag (Fulltag y))
                    (cond ((eq tag fulltag-misc)
                           (ccl-funcall (ccl 'hairy-equal) x y))
                          (t nil))))))))

(deflapfunction %type-of (x)
  (let ((type (%type-name-of x)))
    (if (eq type 'lock)
      (gvref x 1)
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
  (let* ((loword (logior (ash (logand hi #xF) 28) low))
         (hiword (ash hi -4)))
    (assert (eql (uvector-subtag dfloat) subtag-double-float))
    (setf (uvref dfloat 0) loword)
    (setf (uvref dfloat 1) (logior (logand sign (ash 1 31))
                                   (ash exp 20)
                                   hiword))))

;; (ccl::add-bignum-and-fixnum  #(0 0 1) -1)  #(0 0 1) is (ash 1 64)

(deflapfunction %int-to-dfloat (int dfloat)
  (check-type int ccl-fixnum)
  (check-type dfloat ccl-double-float)
  (ccl-double-float (coerce int 'double-float) dfloat))


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
  (%new-htab size))

;; I give up, everybody wants to use this, let them
;;;  *** TODO back out of changes of putting more stuff in nfasload to avoid defining this
(deflapfunction %get-htab-symbol (string len htab)
  (assert (eq len (uvsize string)))
  (let ((hashkey (native-string string)))
    (multiple-value-bind (symv found-p) (gethash hashkey htab)
      (when found-p
        (values found-p (symvector-sym symv))))))

(deflapfunction %find-symbol (string len package)
  (check-type string ccl-simple-string)
  (assert (eq len (uvsize string)))
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
  (check-type module ccl-simple-string)
  (pushnew module (sym-value (ccl'*modules*)) :test 'uvector-equal))


(deflapfunction %class-of-instance (instance)
  (gvref (gvref instance instance.class-wrapper) %wrapper.class))

(deflapfunction class-of (object)
  (let ((info (gvref (sym-value (ccl '*class-table*))
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
  (if (ccl-simple-string-p uvector)
    (unless (characterp val) (setq val (code-char val))))
  (with-uvector-data (data uvector)
    (error "Should implement ~s" `(heap-vector-init ,val ,uvector))
    (loop for i from 0 below (length data) do (setf (svref data i) val))))



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
  (let ((dt (gvref self 2))
        (dcode (gvref self 3)))
    (apply-in-environment env dcode (list dt args))))

(%defvar (ccl '*gf-proto*) nil 'variable (sym-func (ccl 'gag-any-arg)))

(def-gf-proto gag-one-arg (env self args)
  (assert (eql (length args) 1))
  (let ((dt (gvref self 2))
        (dcode (gvref self 3)))
    (apply-in-environment env dcode (list* dt args))))

(def-gf-proto gag-two-arg (env self args)
  (assert (eql (length args) 2))
  (let ((dt (gvref self 2))
        (dcode (gvref self 3)))
    (apply-in-environment env dcode (list* dt args))))

(def-gf-proto funcallable-trampoline (env self args)
  (let ((dcode (gvref self 3)))
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
                    (let* ((data (gvector-data self))
                           (thing (svref data 0))
                           (dcode (svref data 1)))
                      (funcall-in-environment env dcode thing args)))))

(deflapfunction cvm-make-reader-method (slot-id lookup name bits)
  (make-cloned-fn 'reader-method
                  (vector slot-id lookup name bits)
                  (named-function reader-method-code (env self args)
                    (destructuring-bind (instance) args
                      (let* ((data (gvector-data self))
                             (slot-id (svref data 0)))
                        (funcall-in-environment env (svref data 1) instance slot-id))))))


(deflapfunction cvm-make-writer-method (slot-id lookup name bits)
  (make-cloned-fn 'writer-method
                  (vector slot-id lookup name bits)
                  (named-function cvm-writer-method-code (env self args)
                    (destructuring-bind (new instance) args
                      (let* ((data (gvector-data self))
                             (slot-id (svref data 0)))
                        (funcall-in-environment env (svref data 1)
                                                instance slot-id new))))))


(deflapfunction cvm-make-slot-lookup-fn (map table bits)
  (make-cloned-fn 'slot-lookup
                  (vector map table bits)
                  (named-function cvm-slot-lookup-fn-code (env self args)
                    (declare (ignore env))
                    (destructuring-bind (slot-id) args
                      (let* ((data (gvector-data self))
                             (table (svref data 1))
                             (index (gvref slot-id slot-id.index)))
                        (gvref table
                               (let ((map-data (svref data 0)))
                                 (if (< index (length map-data)) (svref map-data index) 0))))))))

(deflapfunction cvm-make-slot-getter (map table class lookup missing bits)
  (make-cloned-fn 'slot-getter
                  (vector map table class lookup missing bits)
                  (named-function cvm-slot-getter-code (env self args)
                    (destructuring-bind (instance slot-id) args
                      (let* ((data (gvector-data self))
                             (map-data (gvector-data (svref data 0)))
                             (table (svref data 1))
                             (index (gvref slot-id slot-id.index)))
                        (if (or (>= index (length map-data))
                                (eql 0 (setq index (svref map-data index))))
                          (funcall-in-environment env (svref data 4)  ;; missing
                                                  instance slot-id)
                          (funcall-in-environment env (svref data 3) ;; lookup using class
                                                  (svref data 2) instance (gvref table index))))))))

(deflapfunction cvm-make-slot-setter (map table class lookup missing bits)
  (make-cloned-fn 'slot-setter
                  (vector map table class lookup missing bits)
                  (named-function cvm-slot-setter-code (env self args)
                    (destructuring-bind (instance slot-id val) args
                      (let* ((data (gvector-data self))
                             (map-data (gvector-data (svref data 0)))
                             (table (svref data 1))
                             (index (gvref slot-id slot-id.index)))
                        (if (or (>= index (length map-data))
                                (eql 0 (setq index (svref map-data index))))
                          (funcall-in-environment env (svref data 4)  ;; missing
                                                  instance slot-id val)
                          (funcall-in-environment env (svref data 3) ;; set using class
                                                  (svref data 2) instance (gvref table index) val)))))))


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
                      (funcall-in-environment env (ccl'%%typep) thing (gvref self 0))))))


(deflapfunction %nth-immediate (fn index)
  (check-type fn ccl-function)
  (gvref fn index))

(deflapfunction %set-nth-immediate (fn index value)
  (check-type fn ccl-function)
  (gvset fn index value))


;; aka Make a heap vector
(deflapfunction fudge-heap-pointer (ptr subtag num-elts)
  (check-type subtag (unsigned-byte 8))
  (check-type num-elts (unsigned-byte 56))
  (let ((ptr (%macptr-ptr ptr)))
    (setf (cffi:mem-ref ptr :uint64) (logior (ash num-elts 8) subtag))
    (make-uvector subtag ptr)))

;; set ptr to point to the actual vector data
(deflapfunction %vect-data-to-macptr (vect ptr)
  (with-uvector-data (data vect)
    (setf (%macptr-value ptr) (+ (cffi:pointer-address data) 8))
    (error "not a heap vector: ~s" vect))
  ptr)

;; set ptr to the address to pass to free
(deflapfunction %%make-disposable (ptr vect)
  (with-uvector-data (data vect)
    (setf (%macptr-value ptr) data)
    (error  "Not a heap vector: ~s" vect)))
