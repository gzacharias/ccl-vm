;;; Writes ffi-records.lisp and ffi-constants.lisp: the CFFI definitions of the foreign records
;;; and the values of the OS constants the VM's runtime needs, for each platform, from that
;;; platform's ccl interface database.  Run it in native ccl:
;;;   (load "gen-ffi.lisp")
;;;   (gen-ffi "/path/to/headers/" "/path/to/ccl-vm/")
;;; where /path/to/headers/ has the libc interface directories of the ccl releases
;;; (darwin-x86-headers64/libc/, darwin-arm64-headers/libc/, x86-headers64/libc/, arm64-headers/libc/).

(in-package "CCL")

(defpackage "CFFI" (:use) (:export "DEFCSTRUCT" "DEFCUNION"))
(defpackage "CCL-FFI" (:use))

;; Record names as the bundle refers to them.
(defparameter *vm-ffi-records* '(:timeval :timespec :rusage :stat :dirent :<D>L_INFO :host_basic_info))

;; Constant names as the bundle refers to them.
(defparameter *vm-ffi-constants*
  '("_SYS_NAMELEN" "RTLD_GLOBAL" "RTLD_NOLOAD" "HOST_BASIC_INFO_COUNT" "KERN_SUCCESS" "HOST_BASIC_INFO"
    "_SC_CLK_TCK" "_SC_PAGESIZE" "PATH_MAX" "S_IFMT" "S_IFDIR" "S_IFREG" "S_IFLNK" "S_IFIFO" "S_IFSOCK"
    "SEEK_CUR" "SEEK_SET" "SEEK_END" "O_RDONLY" "O_WRONLY" "O_RDWR" "O_CREAT" "O_EXCL" "RUSAGE_SELF"
    "EAI_AGAIN" "EAI_FAIL" "EAI_NONAME" "EPERM" "ENOENT" "EINTR" "ENOMEM" "EACCES" "EEXIST" "ENFILE"
    "EMFILE" "ERANGE" "EAGAIN" "EADDRINUSE" "EADDRNOTAVAIL" "ENETDOWN" "ENETUNREACH" "ENETRESET"
    "ECONNABORTED" "ECONNRESET" "ENOBUFS" "ESHUTDOWN" "ETIMEDOUT" "ECONNREFUSED" "EHOSTDOWN"
    "EHOSTUNREACH" "_PC_MAX_INPUT" "SOL_SOCKET" "SO_SNDLOWAT" "AF_INET" "AF_INET6" "AF_UNIX"))

;; (features interface-directory interface-package attributes), as in the backends' ftds.
(defparameter *vm-ffi-platforms*
  '(("(and darwin x86-64)" "ccl:darwin-x86-headers64;" "X86-DARWIN64"
     (:bits-per-word 64 :signed-char t :struct-by-value t :prepend-underscore t))
    ("(and darwin arm64)" "ccl:darwin-arm64-headers;" "ARM64-DARWIN"
     (:bits-per-word 64 :signed-char t :struct-by-value t :natural-alignment t :prepend-underscore nil))
    ("(and linux x86-64)" "ccl:x86-headers64;" "X86-LINUX64"
     (:bits-per-word 64 :struct-by-value t))
    ("(and linux arm64)" "ccl:arm64-headers;" "ARM64-LINUX"
     (:bits-per-word 64 :signed-char nil :struct-by-value t :natural-alignment t :prepend-underscore nil))))

(defun vm-ffi-symbol (name)
  (intern (string name) "CCL-FFI"))

(defun vm-cffi-type (type)
  (etypecase type
    (foreign-integer-type
     (let ((bits (foreign-integer-type-bits type)))
       (assert (member bits '(8 16 32 64)) () "Unexpected integer size ~s" type)
       (intern (format nil "~:[U~;~]INT~d" (foreign-integer-type-signed type) bits) :keyword)))
    (foreign-single-float-type :float)
    (foreign-double-float-type :double)
    ;; Before macptr: records are a kind of macptr type.
    (foreign-record-type
     (assert (foreign-record-type-name type) () "Anonymous record ~s" type)
     (list (foreign-record-type-kind type) (vm-ffi-symbol (foreign-record-type-name type))))
    (foreign-macptr-type :pointer)))

;; Returns the defcstruct/defcunion forms for RECORD, preceded by those for the records it contains.
;; NAME is the name to define it as, when that isn't the record's own (a typedef).
(defun vm-record-forms (record done &optional (name (foreign-record-type-name record)))
  (let ((forms nil))
    (labels ((field-form (field)
               (let ((type (foreign-record-field-type field))
                     (offset (foreign-record-field-offset field))
                     (count nil))
                 (assert (and (zerop (mod offset 8)) (zerop (mod (foreign-record-field-bits field) 8)))
                         () "Bit field ~s in ~s" field record)
                 (when (typep type 'foreign-array-type)
                   (setq count (reduce #'* (foreign-array-type-dimensions type))
                         type (foreign-array-type-element-type type)))
                 (when (typep type 'foreign-record-type)
                   (setq forms (append forms (vm-record-forms type done))))
                 `(,(vm-ffi-symbol (foreign-record-field-name field))
                   ,(vm-cffi-type type)
                   ,@(and count `(:count ,count))
                   :offset ,(/ offset 8)))))
      (progn
        (unless (gethash name done)
          (setf (gethash name done) t)
          (ensure-foreign-type-bits record)
          (let ((fields (mapcar #'field-form (foreign-record-type-fields record))))
            (append forms
                    (list `(,(ecase (foreign-record-type-kind record)
                               (:struct 'cffi:defcstruct)
                               (:union 'cffi:defcunion))
                            (,(vm-ffi-symbol name) :size ,(/ (foreign-record-type-bits record) 8))
                            ,@fields)))))))))

(defun vm-platform-ftd (dir package attributes)
  (let ((ftd (make-ftd :interface-db-directory dir
                       :interface-package-name package
                       :attributes attributes)))
    (unless (find-package package) (make-package package :use nil))
    (install-standard-foreign-types ftd)
    (use-interface-dir :libc nil ftd)
    ftd))

;; Returns the definitions for the platform and the names it doesn't have.
(defun vm-record-definitions (ftd)
  (let* ((*target-ftd* ftd)
         (done (make-hash-table))
         (forms nil)
         (missing nil))
    (dolist (name *vm-ffi-records*)
      (let ((record (or (load-record name)
                        (and (load-foreign-type name)
                             (let ((type (parse-foreign-type name)))
                               (and (typep type 'foreign-record-type)
                                    (or (load-record (foreign-record-type-name type)) type)))))))
        (if record
          (setq forms (append forms (vm-record-forms record done name)))
          (push name missing))))
    (values forms (reverse missing))))

(defun vm-constant-definitions (ftd)
  (let ((*target-ftd* ftd)
        (forms nil)
        (missing nil))
    (dolist (name *vm-ffi-constants*)
      (let ((value (interface-constant-value ftd (intern name (ftd-interface-package-name ftd)))))
        (cond ((null value) (push name missing))
              (t (check-type value integer)
                 (push `(defconstant ,(vm-ffi-symbol name) ,value) forms)))))
    (values (reverse forms) (reverse missing))))

(defun write-vm-ffi-file (output definitions-function separator)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package "COMMON-LISP-USER"))
            (*print-pretty* t)
            (*print-case* :downcase)
            (*print-right-margin* 110))
        (format out ";;; Generated by tools/gen-ffi.lisp from the ccl 1.14 interface databases.  Don't edit.~%")
        (format out "~%(in-package :ccl-vm)~%")
        (loop for (features dir package attributes) in *vm-ffi-platforms*
              do (multiple-value-bind (forms missing)
                     (funcall definitions-function (vm-platform-ftd dir package attributes))
                   (format out "~%~%#+~a~%(progn~{~a~s~})~%" features
                           (loop for form in forms collect separator collect form))
                   (when missing
                     (format out "~%;; Not in ~a: ~{~a~^ ~}~%" dir missing))))))))

(defun gen-ffi (headers-root output-directory)
  (let ((*interfaces-root* (pathname headers-root)))
    (write-vm-ffi-file (merge-pathnames "ffi-records.lisp" output-directory) 'vm-record-definitions (format nil "~%~%"))
    (write-vm-ffi-file (merge-pathnames "ffi-constants.lisp" output-directory) 'vm-constant-definitions (format nil "~%"))))
