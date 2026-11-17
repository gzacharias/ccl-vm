;;; Writes ffi-types.lisp, ffi-records.lisp, ffi-constants.lisp and ffi-functions.lisp: the CFFI
;;; definitions of the foreign types and records, the values of the OS constants and the signatures
;;; of the C functions the VM's runtime needs, for each platform, found by clang (the ffi-info
;;; library).  Load it in a lisp with Quicklisp, ffi-info and clang:
;;;   (load "gen-ffi.lisp")
;;;   (ccl-vm-gen-ffi:gen-ffi "/path/to/ccl-vm/" :linux-x86-64-sysroot "..." :linux-arm64-sysroot "...")
;;; Darwin's headers come from the SDK; Linux's from sysroots with the targets' usr/include.

(defpackage "CCL-VM-GEN-FFI" (:use "COMMON-LISP") (:export "GEN-FFI"))

(in-package "CCL-VM-GEN-FFI")

(defpackage "CCL-FFI" (:use))

;; The generated files are printed in ccl-vm's package.
(defun ccl-vm-package ()
  (or (find-package "CCL-VM") (make-package "CCL-VM" :use '("COMMON-LISP"))))

(defparameter *headers*
  '("sys/types.h" "sys/stat.h" "sys/time.h" "sys/resource.h" "sys/socket.h" "sys/utsname.h" "dirent.h"
    "dlfcn.h" "errno.h" "fcntl.h" "unistd.h" "netdb.h" "limits.h" "stdio.h" "stdlib.h" "string.h" "time.h"
    "pwd.h"))

(defparameter *darwin-headers* '("mach/mach.h"))

;; Integer typedefs used in the VM's function signatures.
(defparameter *types* '("size_t" "ssize_t" "off_t" "mode_t" "host_t" "mach_msg_type_number_t"))

;; Records the bundle refers to.
(defparameter *records* '("timeval" "timespec" "rusage" "stat" "dirent" "Dl_info" "host_basic_info"))

;; Constants the bundle refers to.
(defparameter *constants*
  '("_SYS_NAMELEN" "RTLD_GLOBAL" "RTLD_NOLOAD" "HOST_BASIC_INFO_COUNT" "KERN_SUCCESS" "HOST_BASIC_INFO"
    "_SC_CLK_TCK" "_SC_PAGESIZE" "PATH_MAX" "S_IFMT" "S_IFDIR" "S_IFREG" "S_IFLNK" "S_IFIFO" "S_IFSOCK"
    "SEEK_CUR" "SEEK_SET" "SEEK_END" "O_RDONLY" "O_WRONLY" "O_RDWR" "O_CREAT" "O_EXCL" "RUSAGE_SELF"
    "EAI_AGAIN" "EAI_FAIL" "EAI_NONAME" "EPERM" "ENOENT" "EINTR" "ENOMEM" "EACCES" "EEXIST" "ENFILE"
    "EMFILE" "ERANGE" "EAGAIN" "EADDRINUSE" "EADDRNOTAVAIL" "ENETDOWN" "ENETUNREACH" "ENETRESET"
    "ECONNABORTED" "ECONNRESET" "ENOBUFS" "ESHUTDOWN" "ETIMEDOUT" "ECONNREFUSED" "EHOSTDOWN"
    "EHOSTUNREACH" "_PC_MAX_INPUT" "SOL_SOCKET" "SO_SNDLOWAT" "AF_INET" "AF_INET6" "AF_UNIX"))

;; C functions the bundle calls with #_.
(defparameter *functions*
  '("memset" "memmove" "getdtablesize" "nanosleep" "mach_host_self" "host_info" "getpagesize" "sysconf"
    "getuid" "getenv" "getpwuid_r" "uname" "getcwd" "isatty" "chdir" "fpathconf" "getsockopt" "rename"
    "unlink" "mkdir" "rmdir" "strerror" "getrusage"))

;; (features clang-target sysroot-keyword)
(defparameter *platforms*
  '(("(and darwin x86-64)" "x86_64-apple-macos11" nil)
    ("(and darwin arm64)" "arm64-apple-macos11" nil)
    ("(and linux x86-64)" "x86_64-linux-gnu" :linux-x86-64-sysroot)
    ("(and linux arm64)" "aarch64-linux-gnu" :linux-arm64-sysroot)))

;; The bundle's name for a record, field or type: ccl's, upcased with each run of upper-case
;; letters in <>, e.g. "Dl_info" -> "<D>L_INFO".
(defun record-symbol (c-name)
  (intern (with-output-to-string (out)
            (let ((upper nil))
              (loop for ch across c-name
                    do (cond ((and (upper-case-p ch) (not upper)) (write-char #\< out) (setq upper t))
                             ((and (not (upper-case-p ch)) upper) (write-char #\> out) (setq upper nil)))
                       (write-char (char-upcase ch) out))
              (when upper (write-char #\> out))))
          "CCL-FFI"))

;; The bundle's name for a constant is the C name.
(defun constant-symbol (c-name)
  (intern c-name "CCL-FFI"))

(defun cffi-type (type)
  (if (atom type)
    (ecase type (:pointer :pointer) (:void :void))
    (ecase (first type)
      (:int (destructuring-bind (bits signed) (rest type)
              (intern (format nil "~:[U~;~]INT~d" signed bits) :keyword)))
      (:float (ecase (second type) (32 :float) (64 :double)))
      ((:struct :union) (list (first type) (record-symbol (second type)))))))

(defun field-form (field)
  (destructuring-bind (name type offset) field
    (if (and (consp type) (eq (first type) :array))
      `(,(record-symbol name) ,(cffi-type (second type)) :count ,(third type) :offset ,offset)
      `(,(record-symbol name) ,(cffi-type type) :offset ,offset))))

(defun platform-definitions (info)
  (values
   (loop for (name . type) in (ffi-info:c-info-types info)
         collect `(cffi:defctype ,(record-symbol name) ,(cffi-type type)))
   (loop for (name kind size fields) in (ffi-info:c-info-records info)
         collect `(,(ecase kind (:struct 'cffi:defcstruct) (:union 'cffi:defcunion))
                   (,(record-symbol name) :size ,size)
                   ,@(mapcar #'field-form fields)))
   (loop for (name . value) in (ffi-info:c-info-constants info)
         collect `(defconstant ,(constant-symbol name) ,value))
   (loop for (name symbol return-type arg-types variadic) in (ffi-info:c-info-functions info)
         do (when variadic (error "~a is variadic" name))
         collect `(,(intern "DEF-EXTERNAL-CALL" (ccl-vm-package)) ,(if (string= name symbol) name (list name symbol)) ,(cffi-type return-type)
                    ,@(loop for type in arg-types
                            for i from 1
                            collect (list (intern (format nil "ARG~d" i) "CCL-FFI") (cffi-type type)))))))

(defun write-file (output platforms separator)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (ccl-vm-package))
            (*print-pretty* t)
            (*print-case* :downcase)
            (*print-right-margin* 110))
        (format out ";;; Generated by tools/gen-ffi.lisp, with clang.  Don't edit.~%")
        (format out "~%(in-package :ccl-vm)~%")
        (loop for (features forms missing) in platforms
              do (format out "~%~%#+~a~%(progn~{~a~s~})~%" features
                         (loop for form in forms collect separator collect form))
                 (when missing
                   (format out "~%;; Not on this platform: ~{~a~^ ~}~%" missing)))))))

(defun gen-ffi (output-directory &rest sysroots &key linux-x86-64-sysroot linux-arm64-sysroot)
  (declare (ignore linux-x86-64-sysroot linux-arm64-sysroot))
  (let ((types nil) (records nil) (constants nil) (functions nil))
    (loop for (features target sysroot-key) in *platforms*
          do (let* ((sysroot (and sysroot-key
                                  (or (getf sysroots sysroot-key) (error "No ~s given" sysroot-key))))
                    (darwin (search "apple" target))
                    (info (ffi-info:c-info :headers (append *headers* (and darwin *darwin-headers*))
                                           :target target :sysroot sysroot
                                           :extra-args (and (not darwin) '("-D_GNU_SOURCE"))
                                           :types *types* :records *records* :constants *constants*
                                           :functions *functions*))
                    (missing (ffi-info:c-info-missing info)))
               (multiple-value-bind (type-forms record-forms constant-forms function-forms) (platform-definitions info)
                 (push (list features function-forms (remove-if-not (lambda (name) (member name missing :test #'string=)) *functions*)) functions)
                 (push (list features type-forms (remove-if-not (lambda (name) (member name missing :test #'string=)) *types*)) types)
                 (push (list features record-forms (remove-if-not (lambda (name) (member name missing :test #'string=)) *records*)) records)
                 (push (list features constant-forms (remove-if-not (lambda (name) (member name missing :test #'string=)) *constants*)) constants))))
    (write-file (merge-pathnames "ffi-types.lisp" output-directory) (reverse types) (format nil "~%"))
    (write-file (merge-pathnames "ffi-records.lisp" output-directory) (reverse records) (format nil "~%~%"))
    (write-file (merge-pathnames "ffi-constants.lisp" output-directory) (reverse constants) (format nil "~%"))
    (write-file (merge-pathnames "ffi-functions.lisp" output-directory) (reverse functions) (format nil "~%~%"))))
