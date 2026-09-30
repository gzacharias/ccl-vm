(in-package :ccl-vm)

(defpackage "CCL-FFI" (:use))

(defparameter *lap-functions* nil)


;;;; TODO: have some way to put lap functions in the CCL sources rather than the VM?
;;;;   any reason to do that other than having the same structure as other backends?

(defun register-lap-function (&rest args)
  (if *loading-ccl*
    (push args *lap-functions*)
    ;; If deflapfunction is evaluated by other than wholesale reloading of the VM, then
    ;; just update the defn in the vm that exists
    (install-lap-function args)))

(defun init-lap-functions ()
  (loop while *lap-functions* do (install-lap-function (pop *lap-functions*)))
  (makunbound '*lap-functions*)) ;; shouldn't ever reference this once initialized

(defun install-lap-function (args)
  (destructuring-bind (name bits type native-fn) args
    (let* ((_name (ccl-symbol name))
           (_fn (sym-fboundp _name)))
      (when (and _fn *loading-ccl*) ;; means duplicate def...
        (error "~s is already defined as ~s" _name _fn))
      (when (null _fn)
        (setf (sym-func _name) (setq _fn (cons-ccl-function))))
      (setf (ccl-function-data _fn) (vector _name bits)) ;; could add arg info...
      (setf (ccl-function-bclambda _fn) type)
      (setf (ccl-function-native-fn _fn) native-fn))))

(defmacro deflapfunction (name args-or-lap-name &body body)
  (if (and (symbolp args-or-lap-name) (null body))
    `(register-lap-function ',name 0 'lap #',args-or-lap-name)
    (let* ((lap-fn (intern (concatenate 'string "LAP-" (string name)) *native-package*))
           (env-p (member '&environment args-or-lap-name))
           (inner-args (if env-p (butlast args-or-lap-name 2) args-or-lap-name))
           (bits (let ((opt-pos (position '&optional inner-args))
                       (rest-pos (position '&rest inner-args)))
                   (dpb (or opt-pos rest-pos (length inner-args)) $lfbits-numreq
                        (dpb (if opt-pos (- (or rest-pos (length inner-args)) opt-pos 1) 0) $lfbits-numopt
                             (if rest-pos (ash 1 $lfbits-rest-bit) 0))))))
      (assert (null (set-difference (intersection inner-args lambda-list-keywords) '(&optional &rest))))
      `(progn
         ,(if env-p
            (let ((outer-args (list (cadr env-p) (gensym) (gensym))))
              (assert (eql (length env-p) 2))
              `(defun ,lap-fn ,outer-args
                 (declare (ignore ,(cadr outer-args))) ;; self
                 (destructuring-bind ,inner-args ,(caddr outer-args)
                   ,@body)))
            `(defun ,lap-fn ,inner-args ,@body))
         (register-lap-function ',name ,bits ',(if env-p 'lap-with-env 'lap) #',lap-fn)))))


(deflapfunction fout (string &rest values)
  (fresh-line *trace-output*)
  (apply #'format *trace-output* (native-string string) values))

(deflapfunction fdescribe (obj)
  (describe obj))

(deflapfunction fbreak (str &rest args)
  (let ((*package* *native-package*))
    (apply #'break (native-string str) args)))

(deflapfunction %fasload (namestring)
  (let* ((filename (native-string namestring)))
    (assert (equal (pathname-type filename) "bc"))
    (if (probe-file filename)
      (progn (cvmload filename) t)
      (progn
        (if (and *loading-ccl*
                 #+CCL (member (pathname-name filename) ccl::*modules-not-for-cvm* :test 'string-equal))
          (format t "~&***SKIPPING ~s" filename)
          (error "~s not found" namestring))
        nil))))


;;; ** TODO: do in lap for now so can move on, but need to figure out why it's so slow.
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
#-CCL
(deflapfunction soname-from-mach-header (header)
  (declare (ignore header))
  nil)


(deflapfunction cvm-gvectorp (obj)
  (and (ccl-uvector-p obj)
       (gvector-type-p (uvector-subtag obj))))


(deflapfunction cvm-symbolp (obj)
  (or (null obj)
      (eq obj t)
      (ccl-symvector-p obj)))

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
  ;; So far only used for (%current-tcr) which is a fixnum.
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


(deflapfunction %macptr-domain (macptr)
  ;; this seems to be a raw value that gets boxed by x8664 %macptr-domain!
  (uvref macptr macptr.domain))

(deflapfunction %set-macptr-domain (macptr val)
  ;; x8664 unboxes the value before storing it in the uvector!! but it's a value like 1,
  (check-type val ccl-fixnum)
  (uvset macptr macptr.domain val))

;; x8664 does the same unbox/unboxing here.  Is this for the kernel too look at or something?
(deflapfunction %macptr-type (macptr)
  (uvref macptr macptr.type))

(deflapfunction %set-macptr-type (macptr val)
  (check-type val ccl-fixnum)
  (uvset macptr macptr.type val))

;; 'weak-gc-method  'batch-flag 'all-areas 'tenured-area 'statically-linked 'host-platform 'batch-flag
;; static-cons-area free-static-conses ret1valaddr 'ppc::altivec-present 'stack-size 'default-allocation-quantum
;; 'oldest-ephemeral
(deflapfunction cvm-get-kernel-global (name)
  (check-type name ccl-symvector)
  (cond ((eq name (ccl'batch-flag))    0) ;; don't want batch mode
        ;; Known requests... what to do?
        ((or (eq name (ccl 'stack-size))
             (eq name (ccl 'default-allocation-quantum)))
         37)
        (t (warn "Trying to get native value of ~s" name)
           73)))

(defvar *fake-heap-image-name* nil)
(defvar *fake-argv* (cffi:foreign-alloc :pointer :count 0 :null-terminated-p t))

(deflapfunction cvm-get-kernel-global-ptr (name dest)
  (check-type name ccl-symvector)
  (check-type dest ccl-macptr)
  (setf (%macptr-value dest)
        (cond ((eq name (ccl'image-name))
               (or *fake-heap-image-name*
                   (setq *fake-heap-image-name*
                         (cffi:foreign-string-alloc 
                          ;; This is used only to set the CCL: logical name. It must be a file that exists,
                          ;; inside the ccl directory (if we just use the directory, last component gets stripped)
                          (namestring (make-pathname :name "level-0" :defaults *CCL-DIRECTORY*))))))
              ((eq name (ccl'argv)) *fake-argv*) ;;; *** TODO
              ;; Known requests... what to do?
              ((or (eq name (ccl 'area-lock))
                   (eq name (ccl 'exception-lock)))
               39)
              (t (warn "Trying to get native value of ~s (into ptr)" name)
                 93)))
  dest)

#+hemlock(hemlock::defindent "defcstruct" 1)


(cffi:defcstruct ccl-ffi::<d>l_info
  (ccl-ffi::dli_fname :pointer)
  (ccl-ffi::dli_fbase :pointer)
  (ccl-ffi::dli_sname :pointer)
  (ccl-ffi::dli_saddr :pointer))

(cffi:defcstruct ccl-ffi::timeval
  (ccl-ffi::tv_sec :int64)
  (ccl-ffi::tv_usec :int32))

(cffi:defcstruct ccl-ffi::host_basic_info
  (ccl-ffi::max_cpus :int32)
  (ccl-ffi::avail_cpus :int32)
  (ccl-ffi::memory_size :uint32)
  (ccl-ffi::cpu_type :int32)
  (ccl-ffi::cpu_subtype :int32)
  (ccl-ffi::cpu_threadtype :int32)
  (ccl-ffi::physical_cpu :int32)
  (ccl-ffi::physical_cpu_max :int32)
  (ccl-ffi::logical_cpu :int32)
  (ccl-ffi::logical_cpu_max :int32)
  (ccl-ffi::max_mem :uint64))

(cffi:defcstruct ccl-ffi::timespec
  (ccl-ffi::tv_sec :int64)
  (ccl-ffi::tv_nsec :int64))

(cffi:defcstruct ccl-ffi::stat
  (ccl-ffi::st_dev :int32)
  (ccl-ffi::st_mode :uint16)
  (ccl-ffi::st_nlink :uint16)
  (ccl-ffi::st_ino :uint64)
  (ccl-ffi::st_uid :uint32)
  (ccl-ffi::st_gid :uint32)
  (ccl-ffi::st_rdev :int32)
  (ccl-ffi::st_atimespec (:struct ccl-ffi::timespec))
  (ccl-ffi::st_mtimespec (:struct ccl-ffi::timespec))
  (ccl-ffi::st_ctimespec (:struct ccl-ffi::timespec))
  (ccl-ffi::st_birthtimespec (:struct ccl-ffi::timespec))
  (ccl-ffi::st_size :int64)
  (ccl-ffi::st_blocks :int64)
  (ccl-ffi::st_blksize :int32)
  (ccl-ffi::st_flags :uint32)
  (ccl-ffi::st_gen :uint32)
  (ccl-ffi::st_lspare :int32)
  (ccl-ffi::st_qspare1 :int64)
  (ccl-ffi::st_qspare2 :int64))

(cffi:defctype ccl-ffi::mach_msg_type_number_t :uint32)


(cffi:defcstruct ccl-ffi::dirent
  (ccl-ffi::d_ino :uint64)
  (ccl-ffi::d_seekoff :uint64)
  (ccl-ffi::d_reclen :uint16)
  (ccl-ffi::d_namlen :uint16)
  (ccl-ffi::d_type :uint8)
  (ccl-ffi::d_name :uint8 :count 1024))

;; This is what we seem to get from ff-readdir
(cffi:defcstruct ccl-ffi::dirent32
  (ccl-ffi::d_ino :uint32)
  (ccl-ffi::d_reclen :uint16)
  (ccl-ffi::d_type :uint8)
  (ccl-ffi::d_namlen :uint8)
  (ccl-ffi::d_name :uint8 :count 1024))


(defun cffi-symbol (sym)
  (intern (sym-native-pname sym) :ccl-ffi))

(defun cffi-type-name (sym)
  ;; CFFI complains if we don't wrap (:struct) around struct types, but offers no
  ;; way to tell if something is a struct without triggering the complaint.
  (let ((type-name (cffi-symbol sym)))
    (handler-case (cffi::parse-type `(:struct ,type-name))
      (cffi::undefined-foreign-type-error () type-name))))


(deflapfunction cvm-foreign-bit-size (rec-spec)
  (destructuring-bind (type . accessors) rec-spec
    (assert (null accessors)) ;; true for now
    (* 8 (cffi:foreign-type-size (cffi-type-name type)))))


;;; *** TODO: I think the change I made to accept  record.field in record-size might be confused as to whether you are looking
;;;   at an embedded structure or a pointer to a structure?  CHeck it out.

;; CFFI patch.  Make it so CFFI:FOREIGN-SLOT-COUNT doesn't err out on non-aggregate fields, just returns 1.
; Not needed after all?
;(defmethod cffi::slot-count ((slot t)) 1)

(deflapfunction cvm-access-foreign-field (ccl-ptr path bit-offset)
  (cassert (eql 0 bit-offset))
  (assert path)
  (let* ((ptr (%macptr-ptr ccl-ptr))
         (stype (cffi-symbol (pop path)))
         (offset 0))
    (if (null path)
      (cffi:mem-ref ptr stype)
      (loop 
        for type = `(:struct ,stype) then (cffi:foreign-slot-type type slot-name)
        for slot-name = (cffi-symbol (pop path))
        while path
        do (incf offset (cffi:foreign-slot-offset type slot-name))
        finally (return (ccl (cffi:foreign-slot-value (cffi:inc-pointer ptr offset) type slot-name)))))))

(deflapfunction setf-cvm-access-foreign-field (ccl-ptr path bit-offset value)
  (cassert (eql 0 bit-offset))
  (when (null value) (error "BUG: how is value null?"))
  (let* ((ptr (%macptr-ptr ccl-ptr))
         (cvalue (if (ccl-macptr-p value) (%macptr-ptr value) value))
         (stype (cffi-symbol (if (consp path) (pop path) (prog1 path (setq path nil)))))
         (offset 0))
    (if (null path)
      (setf (cffi:mem-ref ptr stype) cvalue)
      (loop
        for type = `(:struct ,stype) then (cffi:foreign-slot-type type slot-name)
        for slot-name = (cffi-symbol (pop path))
        while path
        do (incf offset (cffi:foreign-slot-offset type slot-name))
        finally (setf (cffi:foreign-slot-value (cffi:inc-pointer ptr offset) type slot-name) cvalue))))
  value)


(defconstant u32-mask #xFFFFFFFF)

(declaim (inline u32-sign-extend))
(defun u32-sign-extend (word)
  (if (logbitp 31 word) (logior word (ash -1 32)) word))

;;; Predefine some foreign fns we call during startup, figure out dynamic stuff later
(cffi:defctype ccl-ffi::host_t :uint32)
(cffi:defctype ccl-ffi::size_t :uint64)
(cffi:defctype ccl-ffi::ssize_t :int64)
(cffi:defctype ccl-ffi::offset_t :int64)


(defconstant CCL-FFI::_SYS_NAMELEN 256)
(defconstant CCL-FFI::RTLD_GLOBAL 8)
(defconstant CCL-FFI::RTLD_NOLOAD 16)
(defconstant CCL-FFI::HOST_BASIC_INFO_COUNT 12)
(defconstant CCL-FFI::KERN_SUCCESS 0)
(defconstant CCL-FFI::HOST_BASIC_INFO 1)
(defconstant CCL-FFI::_SC_CLK_TCK 3)
(defconstant CCL-FFI::_SC_PAGESIZE 29)
(defconstant CCL-FFI::PATH_MAX 1024)
(defconstant CCL-FFI::S_IFMT  #xF000)
(defconstant CCL-FFI::S_IFDIR #x4000)
(defconstant CCL-FFI::S_IFREG #x8000)
(defconstant CCL-FFI::S_IFLNK #xA000)
(defconstant CCL-FFI::S_IFIFO #x1000)
(defconstant CCL-FFI::SEEK_CUR 1)
(defconstant CCL-FFI::SEEK_SET 0)
(defconstant CCL-FFI::O_RDONLY 0)
(defconstant CCL-FFI::O_WRONLY 1)
(defconstant CCL-FFI::O_RDWR 2)
(defconstant CCL-FFI::O_CREAT #x200)
(defconstant CCL-FFI::O_EXCL #x800)
(defconstant CCL-FFI::EAI_AGAIN 2)
(defconstant CCL-FFI::EAI_FAIL 4)
(defconstant CCL-FFI::EAI_NONAME 8)
(defconstant CCL-FFI::EPERM 1)
(defconstant CCL-FFI::ENOENT 2)
(defconstant CCL-FFI::EINTR 4)
(defconstant CCL-FFI::ENOMEM 12)
(defconstant CCL-FFI::EACCES 13)
(defconstant CCL-FFI::EEXIST 17)
(defconstant CCL-FFI::ENFILE 23)
(defconstant CCL-FFI::EMFILE 24)
(defconstant CCL-FFI::ERANGE 34)
(defconstant CCL-FFI::EAGAIN 35)
(defconstant CCL-FFI::EADDRINUSE 48)
(defconstant CCL-FFI::EADDRNOTAVAIL 49)
(defconstant CCL-FFI::ENETDOWN 50)
(defconstant CCL-FFI::ENETUNREACH 51)
(defconstant CCL-FFI::ENETRESET 52)
(defconstant CCL-FFI::ECONNABORTED 53)
(defconstant CCL-FFI::ECONNRESET 54)
(defconstant CCL-FFI::ENOBUFS 55)
(defconstant CCL-FFI::ESHUTDOWN 58)
(defconstant CCL-FFI::ETIMEDOUT 60)
(defconstant CCL-FFI::ECONNREFUSED 61)
(defconstant CCL-FFI::EHOSTDOWN 64)
(defconstant CCL-FFI::EHOSTUNREACH 65)
(defconstant CCL-FFI::_PC_MAX_INPUT 3)
(defconstant CCL-FFI::S_IFSOCK #xC000)
(defconstant CCL-FFI::SOL_SOCKET #xFFFF)
(defconstant CCL-FFI::SO_SNDLOWAT #x1003)
(defconstant CCL-FFI::AF_INET 2)
(defconstant CCL-FFI::AF_INET6 30)
(defconstant CCL-FFI::AF_UNIX 1)



(deflapfunction cvm-os-constant (symvec)
  (check-type symvec ccl-symvector)
  (let* ((sym (cffi-symbol symvec)))
    (loop until (boundp sym)
      do (cerror "Try again" "Don't know how to get OS constant ~s" symvec))
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

(def-external-call "uname" :int
  (buf :pointer))

(def-external-call "getcwd" :pointer
  (buf :pointer)
  (size ccl-ffi::size_t))

(def-external-call "isatty" :int
  (fd :int))

(def-external-call "chdir" :int
  (path :pointer))

(def-external-call "fpathconf" :long
  (fd :int)
  (size :int))

(def-external-call "getsockopt" :int
  (fd :int)
  (level :int)
  (option :int)
  (opt_value :pointer)
  (opt_len :pointer))

(def-external-call "rename" :int
  (old :pointer)
  (new :pointer))

(def-external-call "unlink" :int
  (path :pointer))

(def-external-call "strerror" :pointer
  (errno :int))

;; TODO: see if can make this an alist (or hash table) based on the CCL sym rather than the native sym!
;;  Only thing is, would need to clear it out any time clear out the package system.
(defun get-external-fn (sym)
  (let ((name (sym-native-pname sym)))
    (or (cdr (assoc name *known-c-functions-alist* :test 'equal))
        (progn
          (cerror "try again" "unknown external fn ~s (~s)" sym (cffi:foreign-symbol-pointer name))
          (get-external-fn sym)))))

(deflapfunction cvm-external-call (sym &rest args)
  (assert (eq (sym-pkg sym) *ffi-pkg*))
  #+use-fcell ;; figure out caching later.  Can't put it in the sym fcell, because that contains the macro defn.
  (let ((ffn (sym-fboundp sym)))
    ;; TODO: Could init all the functions first time this is called, then set *known-c-functions-alist* to nil

    ;; So this defines SYM as a ccl-function whose native function invokes the lisp ccl-ffi::sym fn defined by cffi:defcfun.
    ;; However, #_sym wants to define a MACRO.

    ;; %EXTERNAL-CALL-EXPANDER, in defered case, expands into (cvm-external-call ',name ,@args)

    ;;; #_-reader, makes a DEF that is lookup of sym in external-function-definitions of the FTD
    ;;;    if have DEF and macro-function of SYM is #'%external-call-expander, then just returns the sym
    ;;;   else LOAD-EXTERNAL-FUNCTION.  So basically it's just reading the symbol with thhe side effect of
    ;;;  making sure the symbol has a macro definition of #'%external-call-expander, so when it's compiled/evaluated,
    ;;;  

    ;;; load-external-function, looks up def, which in our case is `(deferred-function-definition ,sym)
    ;;;   records this def for the symbol, but also sets macro-function of SYM to be '%external-call-expander.
    ;;;
    (unless ffn
      (setf (sym-func sym)
            (setq ffn (make-cloned-fn 'lap
                                      (vector sym (dpb (length args) $lfbits-numreq 0))
                                      (get-external-fn sym)))))
    (ccl-apply ffn args))
  #-use-fcell
  (let ((fn (get-external-fn sym)))
    (apply fn args)))


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


(deflapfunction %iash (digit count)
  (ash digit count))


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

(deflapfunction %bignum-sign-bits (bignum)
  (let ((high (uvref bignum (1- (uvsize bignum)))))
    (- 32 (integer-length (if (logbitp 31 high) (lognot high) high)))))

(deflapfunction %set-bignum-length (newlen bignum)
  (let ((oldlen (uvsize bignum)))
    (assert (<= newlen oldlen))
    (unless (eql newlen oldlen)
      (with-uvector-data (vec bignum) :error
        (setf (uvector-data bignum) (subseq vec 0 newlen))))))
  
(deflapfunction %bignum-hash (bignum)
  (let* ((len (uvsize bignum))
         (hash (+ (ash len 8) subtag-bignum)))
    (with-uvector-data (vec bignum) :error
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

(deflapfunction %digit-logical-shift-right (digit count)
  (ash digit (- count)))

(deflapfunction %multiply (x y)
  (let ((res (* x y)))
    (values (ash res -32) (logand #xFFFFFFFF res))))

(deflapfunction %multiply-and-add3 (x y carry)
  (let ((res (+ (* x y) carry)))
    (values (ash res -32) (logand #xFFFFFFFF res))))
  

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
  (with-uvector-data (resultv result) :error
    (let* ((val (native-integer bignum))
           (res (* val fixnum)))
      (data-for-bignum res resultv)
      result)))

(defun multiply-and-add-loop (bignum mult result)
  (with-uvector-data (resultv result) :error
    (let* ((val (native-integer bignum))
           (res (* val mult)))
      (data-for-bignum res resultv)
      result)))

(deflapfunction %multiply-and-add-loop64 (x y result i len-y) ;; x[i] * y
  (declare (ignore len-y))
  (with-uvector-data (resultv result) :error
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

;;; This is basically a big hash table of all the CCL objects that are ever stored in an EQ hash table.
;;; **TODO: add a fake address slot to ccl-uvector and get rid of this... Or at least for functions:
;;; this gets big, because there is an eq hash table of lfuns to lfun names (TODO: always leave a slot for lfun
;;; name, so then don't need so much of this).
;;;  ***TODO : who's getting addresses of strings?
;;; CCL-INSTANCE - 4707 CONS - 1787 CCL-SIMPLE-STRING - 2370 CCL-SYMVECTOR - 4260 CCL-FUNCTION - 8169

(defvar-typed *fake-addresses-table* hash-table)

;; for instance hash, the address is just used as an initial hash, but
;; must not conflict with max-class-ordinal, so put things above there
(defconstant min-object-address (ash 1 20))

(deflapfunction strip-tag-to-fixnum (obj)
  (cond ((typep obj 'fixnum) obj)
        ((characterp obj) (char-code obj))
        ((typep obj 'single-float)
         ;; Just put them tegether in any consistent way
         (lap-single-float-bits obj))
        (t (or (gethash obj *fake-addresses-table*)
               (setf (gethash obj *fake-addresses-table*)
                     (+ min-object-address (ash (1+ (hash-table-count *fake-addresses-table*)) 3)))))))


;; This is needed for %print-unreadable-object, unfortunately.
(deflapfunction %address-of (obj)
  (lap-strip-tag-to-fixnum obj))

(deflapfunction cvm-ivector-typecode-p (subtag)
  (ivector-type-p subtag))

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
    #+ccl
    (let ((cres (ccl::fast-mod-3 number divisor recip)))
      (unless (eq res cres)
        (break "fast-mod-3 ~s ~s ~s our ~s ccl ~s"
               number divisor recip res (if (fixnump cres) cres (list 'bogus (ccl::strip-tag-to-fixnum cres))))))
    res))

(deflapfunction %array-header-data-and-offset (array)
  (let ((offset 0))
    (loop while (let ((subtag (uvector-subtag array)))
                  (or (eql subtag subtag-vector-header)
                      (eql subtag subtag-array-header)))
      do (incf offset (gvref array arrayh.displacement))
      do (setq array (gvref array arrayh.data-vector)))
    (values array offset)))

(defun kernel-import-malloc (size)
  (make-ccl-macptr (cffi:foreign-alloc :int8 :count size)))

(defun kernel-import-free (ptr)
  (cffi:foreign-free (%macptr-ptr ptr)))

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

(cffi:defcfun (ff-lstat "lstat$INODE64") :int
  (path :pointer)
  (buf :pointer))

(cffi:defcfun (ff-lseek "lseek") ccl-ffi::offset_t
  (fd :int)
  (offset ccl-ffi::offset_t)
  (whence :int))


(cffi:defcfun (ff-open "open") :int
  (path :pointer)
  (flag :int)
  (mode :uint16))

(cffi:defcfun (ff-close "close") :int
  (fd :int))

(cffi:defcfun (ff-read "read") ccl-ffi::ssize_t
  (fd :int)
  (buf :pointer)
  (count ccl-ffi::size_t))

(cffi:defcfun (ff-write "write") ccl-ffi::ssize_t
  (fd :int)
  (buf :pointer)
  (count ccl-ffi::size_t))

(cffi:defcfun (ff-opendir "opendir") :pointer
  (filename :pointer))

(cffi:defcfun (ff-closedir "closedir") :int
  (dir :pointer))

(cffi:defcfun (ff-readdir "readdir") :pointer
  (dir :pointer))

(cffi:defcfun (ff-ftruncate "ftruncate") :int
  (fd :int)
  (length ccl-ffi::offset_t))

(defun kernel-import-lisp-lseek (fd offset whence) (ff-lseek fd offset whence))

(defun kernel-import-lisp-open (ptr flags mode) (ff-open (%macptr-ptr ptr) flags mode))

(defun kernel-import-lisp-close (fd) (ff-close fd))

(defun kernel-import-lisp-read (fd buf count) (ff-read fd (%macptr-ptr buf) count))

(defun kernel-import-lisp-write (fd buf count) (ff-write fd (%macptr-ptr buf) count))

(defun kernel-import-lisp-opendir (filename) 
  (make-ccl-macptr (ff-opendir (%macptr-ptr filename))))

(defun kernel-import-lisp-closedir (dir) (ff-closedir (%macptr-ptr dir)))

(defun kernel-import-lisp-readdir (dir)
  (let ((dirent (ff-readdir (%macptr-ptr dir))))
    ;; The caller expects dirent, but we seem to get dirent32
    (unless (cffi:null-pointer-p dirent)
      (unless (eql 0 (cffi:foreign-slot-value dirent '(:struct ccl-ffi::dirent32) 'ccl-ffi::d_namlen))
        ;; we have a dirent32.  The only thing the caller cares about is d_name, so rearrage it so
        ;; d_name appears where they expect it 
        (cffi:incf-pointer dirent (- (cffi:foreign-slot-offset '(:struct ccl-ffi::dirent32) 'ccl-ffi::d_name)
                                     (cffi:foreign-slot-offset '(:struct ccl-ffi::dirent) 'ccl-ffi::d_name)))))
    (make-ccl-macptr dirent)))


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

(defun kernel-import-lisp-lstat (nameptr statptr)
  (assert (not (eql 0 (%macptr-value statptr)))) ;; for debuggging
  (ff-lstat (%macptr-ptr nameptr) (%macptr-ptr statptr)))

(defun kernel-import-lisp-ftruncate (fd length)
  (ff-ftruncate fd length))

#-(or darwin freebsd linux)
(error "%GET-ERRNO: need a way to read errno on this OS")

#+(or darwin freebsd linux)
(deflapfunction %get-errno ()
  (- (cffi:mem-ref (cffi:foreign-funcall #+(or darwin freebsd) "__error"
                                         #+linux "__errno_location"
                                         :pointer)
                   :int)))

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
         (name-ptr (%macptr-ptr name)))
    (when (eq hval 0) (setq hval RTLD_DEFAULT))
    (let ((val (ff-dlsym hval name-ptr)))
      (when (and (eql val 0) (eql (cffi:mem-ref name-ptr :char 0) (char-code #\_)))
        (setq val (ff-dlsym hval (cffi:inc-pointer name-ptr 1))))
      (when (eql val 0)
        (error "Can't find symbol ~s" (cffi:foreign-string-to-lisp name-ptr)))
      val)))

(defconstant node-size 8)

;; No threads, no problem!  Except we have to reverse-engineer the offset
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

;; Give up and accept symbols (nil/t)
(deflapfunction %symptr->symbol (symvector)
  (if (or (eq symvector *nil-sym*) (null symvector)) nil
    (if (or (eq symvector *t-sym*) (eq symvector t)) t
      (require-type symvector 'ccl-symvector))))



(deflapfunction %symptr-value %sym-value)
(deflapfunction %set-symptr-value %set-sym-value)

(deflapfunction %set-hash-table-vector-key (vector index value)
  (gvset vector index value))


(deflapfunction %string-hash (start str len)
  (check-type str ccl-simple-string)
  (with-uvector-data (vec str) :error
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

(deflapfunction get-fpu-mode (&optional (mode nil mode-p))
  #+ccl (ccl (if mode-p
               (ccl:get-fpu-mode (native-symbol mode))
               (ccl:get-fpu-mode)))
  #+sbcl (let ((modes (sb-int:get-floating-point-modes)))
           (if mode-p
             (let ((modekey (native-symbol mode)))
               (if (eq modekey :rounding-mode)
                 (ecase (getf modes :rounding-mode)
                   (:nearest (ccl :nearest))
                   (:positive-infinity (ccl :positive))
                   (:negative-infinity (ccl :negative))
                   (:zero (ccl :zero)))
                 (not (null (find (ecase modekey
                                    ((:overflow :underflow :invalid :inexact) mode)
                                    (:division-by-zero (ccl :divide-by-zero)))
                                  (getf modes :traps))))))
             (let ((traps (getf modes :traps)))
               (list (ccl :rounding-mode) (ecase (getf modes :rounding-mode)
                                            (:nearest (ccl :nearest))
                                            (:positive-infinity (ccl :positive))
                                            (:negative-infinity (ccl :negative))
                                            (:zero (ccl :zero)))
                     (ccl :overflow) (not (null (find :overflow traps)))
                     (ccl :underflow) (not (null (find :underflow traps)))
                     (ccl :division-by-zero) (not (null (find :divide-by-zero traps)))
                     (ccl :invalid) (not (null (find :invalid traps)))
                     (ccl :inexact) (not (null (find :inexact traps)))))))
  #-(or ccl sbcl) (error "GET-FPU-MODE not implemented yet on this system"))

(deflapfunction set-fpu-mode (&rest ccl-keys)
  (let ((keys (native ccl-keys)))
    #+ccl (apply #'ccl:set-fpu-mode keys)
    #-ccl
    (destructuring-bind (&key (rounding-mode :nearest rounding-p)
                              (overflow t overflow-p)
                              (underflow t underflow-p)
                              (division-by-zero t zero-p)
                              (invalid t invalid-p)
                              (inexact t inexact-p))
                        keys
      #+sbcl (apply #'sb-int:set-floating-point-modes
                    (nconc
                     (when rounding-p
                       (list :rounding-mode (ecase rounding-mode
                                              (:nearest :nearest)
                                              (:positive :positive-infinity)
                                              (:negative :negative-infinity)
                                              (:zero :zero))))
                     (when (or overflow-p underflow-p zero-p invalid-p inexact-p)
                       (let ((traps (getf (sb-int:get-floating-point-modes) :traps)))
                         (flet ((frob (key val)
                                  (if val
                                    (pushnew key traps :test 'eq)
                                    (setq traps (remove key traps :test 'eq)))))
                           (when overflow-p (frob :overflow overflow))
                           (when underflow-p (frob :underflow underflow))
                           (when zero-p (frob :divide-by-zero division-by-zero))
                           (when invalid-p (frob :invalid invalid))
                           (when inexact-p (frob :inexact inexact)))
                         (list :traps traps)))))
      #-sbcl (error "SET-FLU-MODE not implemented yet on this  system"))))
               

(deflapfunction single-float-bits (float)
  (multiple-value-bind (sig exp sign) (integer-decode-float float)
    ;;(assert (< sig (ash 1 24)))
    (multiple-value-bind (mantissa bexp)
                         (if (eql sig 0)
                           (values 0 0)
                           (let ((bexp (+ exp 150)))
                             (loop while (< sig (ash 1 23)) do (setq sig (ash sig 1) bexp (1- bexp)))
                             (assert (< bexp 255))
                             (if (<= bexp 0)
                               (let ((shift (- 1 bexp)))
                                 (assert (<= 1 shift 23))
                                 (assert (zerop (ldb (byte shift 0) sig)))
                                 (values (ash sig (- shift)) 0))
                               (values (logandc2 sig (ash 1 23)) bexp))))
      (check-type mantissa (unsigned-byte 23))
      (check-type bexp (unsigned-byte 8))
      (logior (if (eql sign -1) (ash 1 31) 0) (ash bexp 23) mantissa))))

(deflapfunction %short-float-sign (float) (< float 0))

(deflapfunction sfloat-significand-zeros (float)
  (- 23 (integer-length (ldb (byte 23 0) (lap-single-float-bits float)))))

(deflapfunction %short-float-abs (float) (abs float))

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

(deflapfunction %double-float-sign (dfloat)
  (logbitp 31 (uvref dfloat 1)))


(deflapfunction %int-to-dfloat (int dfloat)
  (check-type int ccl-fixnum)
  (check-type dfloat ccl-double-float)
  (ccl-double-float (coerce int 'double-float) dfloat))

(deflapfunction %short-float->double-float (sfloat dfloat)
  (check-type sfloat short-float)
  (check-type dfloat ccl-double-float)
  (ccl-double-float (coerce sfloat 'double-float) dfloat))


(deflapfunction double-float-bits (dfloat)
  (check-type dfloat ccl-double-float)
  (values (uvref dfloat 1) (uvref dfloat 0)))

(deflapfunction %dfloat-hash (dfloat)
  (check-type dfloat ccl-double-float)
  ;; TODO: need to make ccl fixnum, plus shouldn't cons a bignum.
  (logand most-positive-fixnum (+ (ash (uvref dfloat 1) 32) (uvref dfloat 0))))

(deflapfunction dfloat-significand-zeros (dfloat)
  (check-type dfloat ccl-double-float)
  (let ((hi (ldb (byte 20 0) (uvref dfloat 1))))
    (if (eql hi 0)
      (+ 20 (- 32 (integer-length (uvref dfloat 0))))
      (- 20 (integer-length hi)))))

(deflapfunction %%double-float-abs! (dfloat result)
  (setf (uvref result 0) (uvref dfloat 0))
  (setf (uvref result 1) (logandc2 (uvref dfloat 1) (ash 1 31)))
  result)

(deflapfunction %%scale-dfloat! (dfloat int result)
  (ccl-double-float (scale-float (native-double-float dfloat) int) result))

(deflapfunction %copy-double-float (dfloat result)
  (setf (uvref result 0) (uvref dfloat 0))
  (setf (uvref result 1) (uvref dfloat 1))
  result)

;;; stuff that was in nfasload.  Perhaps should load nfasload and just isolate the htab stuff?
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

(deflapfunction %find-pkg (name &optional end)
  (%find-pkg name end))

;; I give up, everybody wants to use this, let them
(deflapfunction %get-htab-symbol (string len htab)
  (assert (<= len (uvsize string)))
  (multiple-value-bind (symv found-p) (%htab-get (%htab-hashkey string len) htab)
    (when found-p
      (values found-p (symvector-sym symv)))))

(deflapfunction %htab-remove-symbol (sym htab index)
  (declare (ignore index))
  (%htab-rem (%htab-hashkey sym) htab (sym-symvector sym)))

(deflapfunction %htab-add-symbol (sym htab index)
  (declare (ignore index))
  (%htab-add (%htab-hashkey sym) htab (sym-symvector sym)))

(deflapfunction %find-symbol (string len package)
  (check-type string ccl-simple-string)
  (unless (eql len (uvsize string))
    (with-uvector-data (data string) :error
      (setq string (make-uvector subtag-simple-string (subseq data 0 len)))))
  (multiple-value-bind (sym where) (find-sym-in-pkg string package)
    (values sym (ccl-symbol where))))

(deflapfunction  %insert-symbol (symbol package i e)
  (declare (ignore i e))
  (add-sym-to-pkg symbol package))

(deflapfunction %add-symbol (pname pkg i e &optional force-export)
  (declare (ignore i e))
  (add-sym-to-pkg (make-ccl-symvector pname) pkg force-export))

(deflapfunction %export-symbol (sym package)
  (export-sym-from-pkg (sym-symvector sym) package)
  t)

(deflapfunction provide (module) ;; bootstrapping version
  (when (ccl-symvector-p module) (setq module  (sym-pname module)))
  (check-type module ccl-simple-string)
  (pushnew module (sym-value (ccl'*modules*)) :test 'uvector-equal))

(deflapfunction set-package (name)
  (setf (sym-value (ccl-symbol '*package*)) (pkg-arg name)))

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
  (with-uvector-data (data uvector) :error
    (loop for i from 0 below (length data) do (setf (svref data i) val))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Generic functions

(defmacro def-gf-proto (name args-or-lap-name &body body)
  `(register-lap-function ',name 0 'gf-proto ,(if (listp args-or-lap-name)
                                                `(named-function ,name ,args-or-lap-name ,@body)
                                                `(function ,args-or-lap-name))))

(def-gf-proto gag-any-arg (env self args)
  (let ((dt (gvref self 2))
        (dcode (gvref self 3)))
    (apply-in-environment env dcode (list dt args))))

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
  ;(signal-error $xnofinfunction self args env)
  (error "Unset FIN function ~s ~s ~s" self args env))

(deflapfunction replace-function-code (target proto)
  (assert (eq (ccl-function-bclambda proto) 'gf-proto))
  (setf (ccl-function-native-fn target) (ccl-function-native-fn proto)))

(defun make-cloned-fn (type data native-fn)
  (check-type type symbol)
  (%make-ccl-function :subtag subtag-function
                      :bclambda type
                      :data data
                      :native-fn native-fn))

(deflapfunction cvm-make-gf (proto &rest data)
  (assert (eq (ccl-function-bclambda proto) 'gf-proto))
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
  (apply-in-environment env func args))

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

;; Here bclambda is coming out of the compiler (as opposed to a BC file), so it's entirely a VM object.
;; We want the BC operators to be native symbols. Fortunately this can be done unambiguously because
;; a bclambda has no unquoted symbols other than operators.  Just have to be careful not to convert
;; any quoted symbols.
(deflapfunction make-bclambda-lfun (ccl-bclambda)
  (labels ((nativize-ops (expr)
             (if (atom expr)
               expr
               (let* ((op (car expr))
                      (sym (and (ccl-symvector-p op) (native-symbol op))))
                 (if (eq sym '$bc-quote)
                   (cons '$bc-quote (cdr expr))
                   (cons (if (not (member sym '(t nil)))
                           (progn
                             (assert (or (eq sym 'bclambda) (starts-with-subseq "$BC-" (symbol-name sym))))
                             sym)
                           (nativize-ops op))
                         (mapcar #'nativize-ops (cdr expr))))))))
    (let ((bclambda (nativize-ops ccl-bclambda)))
      (assert (and (consp bclambda) (eq (car bclambda) 'bclambda)))
      (init-ccl-function (cons-ccl-function) bclambda))))

;; Return bclambda as a VM object, e.g. for fasdumping.  Reverse of make-bclambda-lfun
(deflapfunction lfun-bclambda (fn)
  (labels ((vmify-ops (expr)
             (if (atom expr)
               expr
               (let ((op (car expr)))
                 (if (eq op '$bc-quote)
                   (cons (ccl-symbol '$bc-quote) (cdr expr))
                   (cons (if (and (symbolp op) (not (member op '(t nil))))
                           (progn
                             (assert (or (eq op 'bclambda) (starts-with-subseq "$BC-" (symbol-name op))))
                             (ccl-symbol op))
                           (vmify-ops op))
                         (mapcar #'vmify-ops (cdr expr))))))))
    (vmify-ops (ccl-function-bclambda fn))))

(deflapfunction map-bclambda-immediates (fn thunk)
  (labels ((scan (expr)
             (when (consp expr)
               (if (eq (car expr) '$bc-quote)
                 (ccl-funcall thunk (bc-unquote expr))
                 (mapcar #'scan expr)))))
    (scan (ccl-function-bclambda fn))))


(deflapfunction cvm-xdisassemble (fn)
  (let ((bclambda (ccl-function-bclambda fn)))
    (cond ((consp bclambda)
           (assert (eq (car bclambda) 'bclambda)) ;; native object
           (let ((*print-pretty* t)
                 (*print-right-margin* 200)
                 (*package* *native-package*))
             (print (bclambda-lambda bclambda))))
          (t (disassemble (ccl-function-native-fn fn))))
    nil))


(deflapfunction values (&rest the-values)
  (apply #'values the-values))

;;;; Heap vectors

;;; Ok, so this is used for IO.  Really slows things down.

(deflapfunction fudge-heap-pointer (ptr subtag num-elts) ;; aka Make a heap vector
  (check-type subtag (unsigned-byte 8))
  (check-type num-elts (unsigned-byte 56))
  (unless (svref *subtag-ffi-types* subtag)
    (error "~s heap vectors not supported" (subtag-typekey subtag)))
  (let ((ptr (%macptr-ptr ptr)))
    (setf (cffi:mem-ref ptr :uint64) num-elts)
    (make-uvector subtag (cffi:inc-pointer ptr 8))))


;; set ptr to point to the actual vector data
(deflapfunction %vect-data-to-macptr (vect ptr)
  (with-uvector-data (data vect)
    (setf (%macptr-value ptr) (cffi:pointer-address data))
    (error "not a heap vector: ~s" vect))
  ptr)

;; set ptr to the address to pass to _free
(deflapfunction %%make-disposable (ptr vect)
  (with-uvector-data (data vect)
    (setf (%macptr-value ptr) (- (cffi:pointer-address data) 8))
    (error  "Not a heap vector: ~s" vect)))

(defun heap-vector-uvref (uvec index)
  (let* ((subtag (uvector-subtag uvec))
         (ptr (uvector-data uvec)))
    ;(cassert (< index (cffi:mem-ref ptr :uint64 -8)))
    (if (eq subtag subtag-unsigned-8-bit-vector) ;; io buffer
      (cffi:mem-aref ptr :uint8 index)
      (cffi:mem-aref ptr (svref *subtag-ffi-types* subtag) index))))

(defun heap-vector-uvset (uvec index val)
  (let* ((subtag (uvector-subtag uvec))
         (ptr (uvector-data uvec)))
    ;(cassert (< index (cffi:mem-ref ptr :uint64 -8)))
    #+gz (unless (eq subtag subtag-unsigned-8-bit-vector)
           (break "Why uvset this subtag: ~s ~s" subtag (svref *subtag-ffi-types* subtag)))
    (if (eq subtag subtag-unsigned-8-bit-vector) ;; io buffer
      (setf (cffi:mem-aref ptr :uint8 index) val)
      (setf (cffi:mem-aref ptr (svref *subtag-ffi-types* subtag) index) val))))

(defun heap-vector-uvsize (uvec)
  (let* ((ptr (uvector-data uvec)))
    (cffi:mem-ref ptr :uint64 -8)))




(deflapfunction bogus-thing-p (thing)
  (declare (ignore thing))
  nil)

(deflapfunction %copy-ivector-to-ivector (src src-byte-offset dest dest-byte-offset nbytes)
  (let* ((utype (svref *subtag-ffi-types* (uvector-subtag src))))
    (assert (and utype (eq utype (svref *subtag-ffi-types* (uvector-subtag dest))))) ;; not needed
    ;; Reverse engineer the offsets
    (let* ((src-offset src-byte-offset)
           (dest-offset dest-byte-offset)
           (count nbytes))
      (case utype
        ((:int8 :uint8))
        ((:int16 :uint16)
         (assert (= (logand #b1 src-offset) (logand #b1 dest-offset) (logand #b1 count) 0))
         (setq count (ash count -1) src-offset (ash src-offset -1) dest-offset (ash dest-offset -1)))
        ((:int32 :uint32)
         ;; really just need to make sure when one of them is a string, we convert to characters
         (assert (eq (uvector-subtag src) (uvector-subtag dest)))
         (assert (= (logand #b11 src-offset) (logand #b11 dest-offset) (logand #b11 count) 0))
         (setq count (ash count -2) src-offset (ash src-offset -2) dest-offset (ash dest-offset -2)))
        ((:int64 :uint64)
         (assert (= (logand #b111 src-offset) (logand #b111 dest-offset) (logand #b111 count) 0))
         (setq count (ash count -3) src-offset (ash src-offset -3) dest-offset (ash dest-offset -3)))
        (t (error "Cant copy ~s vectors" (subtag-typekey (uvector-subtag src)))))
      (if (and (eq src dest) (> dest-offset src-offset))
        ;; overlapping copy within one vector, moving up: copy backwards
        (loop for n from (1- count) downto 0
          do (setf (uvref dest (+ dest-offset n)) (uvref src (+ src-offset n))))
        (loop for si upfrom src-offset for di upfrom dest-offset for n from 0 below count
          do (setf (uvref dest di) (uvref src si)))))
    dest))


(deflapfunction get-saved-register-values ()
  (values))


(defvar *ccl-toplevel-func* nil)

(deflapfunction %tcr-toplevel-function (tcr)
  (assert (eql tcr 23)) ;; see $bc-current-tcr
  *ccl-toplevel-func*)

(deflapfunction %set-tcr-toplevel-function (tcr func)
  (assert (eql tcr 23)) ;; see $bc-current-tcr
  (setq *ccl-toplevel-func* func))

(deflapfunction %no-thread-local-binding-marker () 'no-thread-local-binding-marker)


(deflapfunction %frame-backlink (p context)
  (declare (ignore context))
  (when p
    (bcenv-parent p)))

(deflapfunction cfp-lfun (p)
  ;; Second value is PC.  0 makes it call arg-check-call-arguments to get the arg info.
  ;; nil makes it print "???".
  (let ((func (bcenv-func p)))
    (values func
            (if (consp (ccl-function-bclambda func)) 0 nil))))

(deflapfunction arg-check-call-arguments (p func)
  (assert (eq func (bcenv-func p)))
  ;; Currently args are recorded on entry to function, so only get recorded as part of
  ;; the bclambda-lambda.  if we make apply-in-environment do it, then could rely
  ;; on it even for lap.  Except in that case, there is no frame for the lap code, just the
  ;; parent function, sigh.  Maybe should make a little env for lap stuff as well.
  (when (consp (ccl-function-bclambda func))
    (bcenv-args p)))

;; send value is bottom-of-stack-p
(deflapfunction lisp-frame-p (p context)
  (declare (ignore p context))
  t)

(deflapfunction catch-csp-p (p context)
  (declare (ignore p context))
  nil)

(deflapfunction %catch-top (tcr)
  (declare (ignore tcr))
  nil)

(deflapfunction %stack< (p1 p2 &optional context)
  (declare (ignore p1 p2 context))
  nil)

(deflapfunction exception-frame-p (p)
  (declare (ignore p))
  nil)

(deflapfunction index->address (p)
  (declare (ignore p))
  #x1234)


;;;; standard io streams

(defparameter *native-streams* (vector 
                                ; input
                                #+ccl (if (find-package :gui)
                                        ;; In the ide, *stdin/out* uses AltConsole and sucks.
                                        *standard-input*
                                        ccl::*stdin*)
                                #+sbcl sb-sys:*stdin*
                                #-(or ccl sbcl) *standard-input*
                                ; output
                                #+ccl (if (find-package :gui)
                                        *standard-output*
                                        ccl::*stdout*)
                                #+sbcl sb-sys:*stdout*
                                #-(or ccl sbcl) *standard-output*
                                ; error
                                #+ccl ccl::*stderr*
                                #+sbcl sb-sys:*stderr*
                                #-(or ccl sbcl) *error-output*
                               ; tty
                                #+sbcl sb-sys:*tty*
                                #-(or sbcl) *terminal-io*
                                ))

(deflapfunction native-interactive-stream-p (which)
  (interactive-stream-p (aref *native-streams* which)))

(deflapfunction native-stream-read-char (which)
  (read-char (aref *native-streams* which) nil :eof))

(defmethod native-stream-read-byte (which)
  (read-byte (aref *native-streams* which) nil :eof))

(deflapfunction native-stream-unread-char (which char)
  (unread-char char (aref *native-streams* which)))

(deflapfunction native-stream-read-char-no-hang (which)
  (read-char-no-hang (aref *native-streams* which) nil :eof))

(deflapfunction native-stream-write-char (which c)
  (write-char c (aref *native-streams* which)))

(deflapfunction native-stream-line-column (which)
  ;; Assume everybody makes gray streams available to cl-user...
  (cl-user::stream-line-column (aref *native-streams* which)))
  
#+sbcl
(defmethod cl-user::stream-line-column ((stream file-stream))
  (sb-kernel:charpos stream))

(deflapfunction native-stream-set-column (which column)
  (if (eql column 0)
    (fresh-line (aref *native-streams* which))
    ;; This is not a gray streams function
    ;;#+ccl (ccl::stream-set-column  (aref *native-streams* which) column)
    (break "someone is setting column!")))

(deflapfunction native-stream-force-output (which)
  (force-output (aref *native-streams* which)))

(deflapfunction native-stream-finish-output (which)
  (finish-output (aref *native-streams* which)))
