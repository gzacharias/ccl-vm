;;; Writes this platform's section of ffi.lisp: the CFFI definitions of the foreign types and
;;; records, the values of the OS constants and the signatures of the C functions the VM's
;;; runtime needs.  Everything comes from clang: it parses the headers (-ast-dump=json), and every
;;; number is a sizeof, offsetof or constant expression that clang evaluates as an enumerator.
;;; To add or update a platform, on that platform, in a lisp with Quicklisp, with clang:
;;;   (load "gen-ffi.lisp")
;;;   (ccl-vm-gen-ffi:gen-ffi "/path/to/ccl-vm/ffi.lisp")
;;; The other platforms' sections are left alone.  (Since nothing is run, another platform's
;;; section can also be made here, given its headers: :target, :sysroot and :features.)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("com.inuoe.jzon" "cffi") :silent t))

(defpackage "CCL-VM-GEN-FFI" (:use "COMMON-LISP") (:export "GEN-FFI"))

(in-package "CCL-VM-GEN-FFI")

(defpackage "CCL-FFI" (:use))

;;;; Asking clang

(defvar *clang* "clang")

;;; Results

;; A type is one of
;;   (:int BITS SIGNEDP), (:float BITS), :pointer, :void,
;;   (:struct NAME) or (:union NAME)    a record, also in the result's records
;;   (:array TYPE COUNT)                COUNT elements of the innermost element type
(defstruct c-info
  types      ; ((name . type) ...)
  records    ; ((name kind size-in-bytes ((field-name type offset-in-bytes) ...)) ...)
  constants  ; ((name . integer) ...)
  functions  ; ((name symbol return-type (arg-type ...) variadicp) ...): SYMBOL is the linker's name
  missing)   ; names that the headers don't declare (or that aren't integer constants)

;;; Running clang

(defun run-clang (source target sysroot extra-args)
  (uiop:with-temporary-file (:pathname path :type "c" :stream out :direction :output :keep nil)
    (write-string source out)
    (finish-output out)
    (close out)
    (multiple-value-bind (output errors status)
        (uiop:run-program `(,*clang* ,@(and target `("-target" ,target))
                                     ,@(and sysroot `("--sysroot" ,sysroot))
                                     ,@extra-args
                                     "-fsyntax-only" "-w" "-Xclang" "-ast-dump=json"
                                     ,(uiop:native-namestring path))
                          :output :string :error-output :string :ignore-error-status t)
      (values output errors status (uiop:native-namestring path)))))

(defun headers-source (headers)
  (format nil "~{#include <~a>~%~}#include <stddef.h>~%" headers))

;; The top-level declarations (a vector of hash tables) of a translation unit.
(defun translation-unit-decls (headers target sysroot extra-args)
  (multiple-value-bind (json errors status) (run-clang (headers-source headers) target sysroot extra-args)
    (unless (zerop status)
      (error "clang failed on ~s:~%~a" headers errors))
    (gethash "inner" (com.inuoe.jzon:parse json))))

(defun node-kind (node) (gethash "kind" node))
(defun node-name (node) (gethash "name" node))
(defun node-inner (node) (gethash "inner" node))

(defun node-type-string (node)
  (let ((type (gethash "type" node)))
    (or (gethash "desugaredQualType" type) (gethash "qualType" type))))

;;; Evaluating integer constant expressions: each one becomes an enumerator.  An expression that
;;; doesn't compile is NIL (clang's errors give its line).

(defun evaluate-constant-expressions (headers expressions target sysroot extra-args)
  (let ((results (make-array (length expressions) :initial-element nil))
        (bad (make-hash-table)))
    (loop
      (let* ((prefix (headers-source headers))
             (first-line (1+ (count #\Newline prefix)))
             (source (with-output-to-string (s)
                       (write-string prefix s)
                       (loop for expression in expressions
                             for i from 0
                             do (if (gethash i bad)
                                  (format s "~%")
                                  (format s "enum { ffi_info_~d = (~a) };~%" i expression))))))
        (multiple-value-bind (json errors status path) (run-clang source target sysroot extra-args)
          (if (zerop status)
            (progn
              (loop for decl across (gethash "inner" (com.inuoe.jzon:parse json))
                    when (equal (node-kind decl) "EnumDecl")
                      do (loop for enumerator across (or (node-inner decl) #())
                               for name = (node-name enumerator)
                               when (and name (uiop:string-prefix-p "ffi_info_" name))
                                 do (setf (aref results (parse-integer name :start 9))
                                          (parse-integer (enumerator-value enumerator)))))
              (return results))
            (let ((new-bad nil))
              (with-input-from-string (s errors)
                (loop for line = (read-line s nil)
                      while line
                      do (let ((start (search (format nil "~a:" path) line)))
                           (when (and start (search "error:" line))
                             (let* ((line-number (parse-integer line :start (+ start (length path) 1) :junk-allowed t))
                                    (i (- line-number first-line)))
                               (when (and (<= 0 i) (< i (length expressions)) (not (gethash i bad)))
                                 (setf (gethash i bad) t new-bad t)))))))
              (unless new-bad
                (error "clang failed:~%~a" errors)))))))))

(defun enumerator-value (enumerator)
  (labels ((find-value (node)
             (when (hash-table-p node)
               (if (and (equal (node-kind node) "ConstantExpr") (gethash "value" node))
                 (gethash "value" node)
                 (loop for child across (or (node-inner node) #())
                       thereis (find-value child))))))
    (or (find-value enumerator)
        (error "No value for ~s" (node-name enumerator)))))

;;; Classifying types from clang's type strings

(defun strip-qualifiers (string)
  (let ((s (string-trim " " string)))
    (loop for qualifier in '("const " "volatile " "restrict " "__restrict ")
          do (loop for p = (search qualifier s)
                   while p
                   do (setq s (concatenate 'string (subseq s 0 p) (subseq s (+ p (length qualifier)))))))
    (string-trim " " s)))

;; :array, :pointer, :record, :float, :void or :int.
(defun type-category (string)
  (let ((s (strip-qualifiers string)))
    (cond ((and (plusp (length s)) (char= (char s (1- (length s))) #\])) :array)
          ((or (find #\* s) (find #\^ s)) :pointer)
          ((find #\( s) :function)
          ((or (uiop:string-prefix-p "struct " s) (uiop:string-prefix-p "union " s)) :record)
          ((member s '("float" "double" "long double" "_Float16") :test #'string=) :float)
          ((string= s "void") :void)
          (t :int))))

;;; The main entry point

(defun c-info (&key headers target sysroot extra-args types records constants functions)
  "Describe the named C TYPES (typedef names), RECORDS (struct, union or typedef names), CONSTANTS
(macros or enumerators with integer values) and FUNCTIONS declared by HEADERS, for clang's TARGET
triple (default: the host), with SYSROOT as the root for the target's headers."
  (let* ((decls (translation-unit-decls headers target sysroot extra-args))
         (typedefs (make-hash-table :test 'equal))
         ;; Complete records by how to spell their type: "struct foo", or a typedef's name for
         ;; an anonymous one.
         (record-decls (make-hash-table :test 'equal))
         (anonymous-records (make-hash-table :test 'equal))
         (function-decls (make-hash-table :test 'equal))
         (missing nil)
         ;; Expressions to evaluate, each with a function to call with its value.
         (expressions nil)
         (result (make-c-info)))
    (loop for decl across decls
          do (let ((name (node-name decl)))
               (cond ((not (equal (node-kind decl) "RecordDecl")))
                     ((not (gethash "completeDefinition" decl)))
                     ((null name) (setf (gethash (gethash "id" decl) anonymous-records) decl))
                     (t (setf (gethash (format nil "~a ~a" (gethash "tagUsed" decl) name) record-decls) decl)))
               (when name
                 (case (intern (node-kind decl) :keyword)
                   (:|TypedefDecl| (setf (gethash name typedefs) decl)
                    (let ((record (some (lambda (id) (gethash id anonymous-records)) (referenced-decl-ids decl))))
                      (when record (setf (gethash name record-decls) record))))
                   (:|FunctionDecl| (setf (gethash name function-decls) decl))))))
    (labels ((evaluate (expression continuation)
               (push (cons expression continuation) expressions))
             (category (type-string)
               (if (gethash (strip-qualifiers type-string) record-decls) :record (type-category type-string)))
             ;; Describes the C type TYPE-STRING, calling CONTINUATION with the description.
             ;; SPELLING is how to write the type in an expression, when TYPE-STRING won't do.
             (describe-type (type-string continuation &optional (spelling type-string))
               (ecase (category type-string)
                 (:pointer (funcall continuation :pointer))
                 (:void (funcall continuation :void))
                 (:function (error "Function type ~s" type-string))
                 (:float (evaluate (format nil "sizeof(~a) * 8" spelling)
                                   (lambda (bits) (funcall continuation (list :float bits)))))
                 (:int (evaluate (format nil "sizeof(~a) * 8" spelling)
                                 (lambda (bits)
                                   (evaluate (format nil "((~a)-1) < 0" spelling)
                                             (lambda (signed)
                                               (funcall continuation (list :int bits (= signed 1))))))))
                 (:record (let ((key (strip-qualifiers type-string)))
                            (funcall continuation (ensure-record key))))
                 (:array (error "Array type outside a record: ~s" type-string))))
             (record-key (name)
               (cond ((gethash name record-decls) name)
                     ((gethash (format nil "struct ~a" name) record-decls) (format nil "struct ~a" name))
                     ((gethash (format nil "union ~a" name) record-decls) (format nil "union ~a" name))
                     ((gethash name typedefs)
                      (let ((target (strip-qualifiers (gethash "qualType" (gethash "type" (gethash name typedefs))))))
                        (and (gethash target record-decls) target)))))
             ;; Adds the record spelled KEY to the result, as NAME, and returns its type.
             (ensure-record (key &optional (name (subseq key (1+ (or (position #\Space key) -1)))))
               (let* ((decl (or (gethash key record-decls) (error "No definition of ~a" key)))
                      (kind (if (equal (gethash "tagUsed" decl) "union") :union :struct)))
                 (unless (assoc name (c-info-records result) :test #'string=)
                   (ensure-record-entry key name kind decl))
                 (list kind name)))
             (ensure-record-entry (key name kind decl)
               (progn
                 (let* ((entry (list name kind nil nil)))
                   (push entry (c-info-records result))
                   (evaluate (format nil "sizeof(~a)" key) (lambda (size) (setf (third entry) size)))
                   (dolist (field (record-fields decl))
                     (destructuring-bind (field-name . field-type) field
                       (let ((field-entry (list field-name nil nil))
                             (member (format nil "((~a *)0)->~a" key field-name)))
                         (setf (fourth entry) (append (fourth entry) (list field-entry)))
                         (evaluate (format nil "offsetof(~a, ~a)" key field-name)
                                   (lambda (offset) (setf (third field-entry) offset)))
                         (if (eq (category field-type) :array)
                           ;; Count the innermost elements, and describe their type.
                           (let ((element member)
                                 (element-type field-type))
                             (loop while (eq (category element-type) :array)
                                   do (setq element (format nil "~a[0]" element)
                                            element-type (array-element-type-string element-type)))
                             (describe-type element-type
                                            (lambda (type)
                                              (evaluate (format nil "sizeof(~a) / sizeof(~a)" member element)
                                                        (lambda (count)
                                                          (setf (second field-entry) (list :array type count)))))
                                            (format nil "__typeof__(~a)" element)))
                           (describe-type field-type
                                          (lambda (type) (setf (second field-entry) type))
                                          (format nil "__typeof__(~a)" member)))))))))
             ;; ((name . type-string) ...), with the members of anonymous structs and unions in place.
             (record-fields (decl)
               (loop for field across (or (node-inner decl) #())
                     when (equal (node-kind field) "FieldDecl")
                       append (progn
                                (when (gethash "isBitfield" field)
                                  (error "Bit field ~s in ~s" (node-name field) (node-name decl)))
                                (if (or (null (node-name field)) (gethash "isImplicit" field))
                                  (record-fields (anonymous-record-decl decl field))
                                  (list (cons (node-name field) (node-type-string field)))))))
             (anonymous-record-decl (decl field)
               (let ((type (node-type-string field)))
                 (or (loop for inner across (node-inner decl)
                           thereis (and (equal (node-kind inner) "RecordDecl")
                                        (search (gethash "id" inner) (or (gethash "typeAliasDeclId" (gethash "type" field)) ""))
                                        inner))
                     ;; Match by position: the anonymous record precedes its implicit field.
                     (let ((position (position field (node-inner decl))))
                       (loop for i from (1- position) downto 0
                             for inner = (aref (node-inner decl) i)
                             when (equal (node-kind inner) "RecordDecl") return inner))
                     (error "Can't find the anonymous record of ~s" type)))))
      (dolist (name types)
        (if (gethash name typedefs)
          (let ((name name))
            (describe-type name (lambda (type) (push (cons name type) (c-info-types result)))))
          (push name missing)))
      (dolist (name records)
        (let ((key (record-key name)))
          (if key (ensure-record key name) (push name missing))))
      (dolist (name constants)
        (let ((name name))
          (evaluate name (lambda (value) (push (cons name value) (c-info-constants result))))))
      (dolist (name functions)
        (let ((decl (gethash name function-decls)))
          (if (null decl)
            (push name missing)
            (let* ((parameters (loop for p across (or (node-inner decl) #())
                                     when (equal (node-kind p) "ParmVarDecl") collect (node-type-string p)))
                   (entry (list name (strip-user-label-prefix (gethash "mangledName" decl) target) nil
                                (make-list (length parameters)) (and (gethash "variadic" decl) t))))
              (push entry (c-info-functions result))
              (describe-type (function-return-type-string (node-type-string decl))
                             (lambda (type) (setf (third entry) type)))
              (loop for parameter in parameters
                    for cell on (fourth entry)
                    do (let ((cell cell))
                         (describe-type (if (eq (category parameter) :array) "void *" parameter)
                                        (lambda (type) (setf (car cell) type)))))))))
      ;; Evaluating may add expressions (via the continuations), so go round until there are none.
      (loop while expressions
            do (let* ((batch (reverse expressions))
                      (results (progn (setq expressions nil)
                                     (evaluate-constant-expressions headers (mapcar #'car batch)
                                                                    target sysroot extra-args))))
                 (loop for (expression . continuation) in batch
                       for value across results
                       do (if value
                            (funcall continuation value)
                            (if (member expression constants :test #'string=)
                              (push expression missing)
                              (error "Can't evaluate ~a" expression)))))))
    (setf (c-info-types result) (order-like types (c-info-types result))
          (c-info-constants result) (order-like constants (c-info-constants result))
          (c-info-functions result) (order-like functions (c-info-functions result))
          (c-info-records result) (reverse (c-info-records result))
          (c-info-missing result) (reverse missing))
    result))

;; The ids of the declarations that NODE's type refers to.
(defun referenced-decl-ids (node)
  (let ((ids nil))
    (labels ((walk (n)
               (when (hash-table-p n)
                 (dolist (key '("decl" "ownedTagDecl"))
                   (let ((ref (gethash key n)))
                     (when (hash-table-p ref) (push (gethash "id" ref) ids))))
                 (loop for child across (or (node-inner n) #()) do (walk child)))))
      (walk node))
    ids))

(defun order-like (names alist)
  (loop for name in names
        for entry = (assoc name alist :test #'string=)
        when entry collect entry))

(defun array-element-type-string (array-type-string)
  (let ((s (strip-qualifiers array-type-string)))
    (subseq s 0 (position #\[ s :from-end t))))

;; The return type in a function type string "RET (ARGS)".
(defun function-return-type-string (function-type-string)
  (let ((depth 0))
    (loop for i from (1- (length function-type-string)) downto 0
          for c = (char function-type-string i)
          do (case c
               (#\) (incf depth))
               (#\( (decf depth)
                (when (zerop depth)
                  (return-from function-return-type-string
                    (string-trim " " (subseq function-type-string 0 i)))))))
    (error "Not a function type: ~s" function-type-string)))

;; Darwin's linker names have a leading underscore that dlsym doesn't want.
(defun strip-user-label-prefix (name target)
  (if (and name (plusp (length name)) (char= (char name 0) #\_)
           (search "apple" (or target (uiop:run-program (list *clang* "-dumpmachine") :output '(:string :stripped t)))))
    (subseq name 1)
    name))

;;;; ccl-vm's files

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

;; This lisp's platform, as features for ffi.lisp's #+.
(defun host-features ()
  (format nil "(and ~(~a~) ~(~a~))"
          (or (find-if (lambda (f) (member f *features*)) '(:darwin :linux :freebsd :netbsd :openbsd :solaris))
              (error "Unknown OS"))
          (or (find-if (lambda (f) (member f *features*)) '(:x86-64 :arm64))
              (error "Unknown architecture"))))

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
   (loop for (name . type) in (c-info-types info)
         collect `(cffi:defctype ,(record-symbol name) ,(cffi-type type)))
   (loop for (name kind size fields) in (c-info-records info)
         collect `(,(ecase kind (:struct 'cffi:defcstruct) (:union 'cffi:defcunion))
                   (,(record-symbol name) :size ,size)
                   ,@(mapcar #'field-form fields)))
   (loop for (name . value) in (c-info-constants info)
         collect `(defconstant ,(constant-symbol name) ,value))
   (loop for (name symbol return-type arg-types variadic) in (c-info-functions info)
         do (when variadic (error "~a is variadic" name))
         collect `(,(intern "DEF-EXTERNAL-CALL" (ccl-vm-package)) ,(if (string= name symbol) name (list name symbol)) ,(cffi-type return-type)
                    ,@(loop for type in arg-types
                            for i from 1
                            collect (list (intern (format nil "ARG~d" i) "CCL-FFI") (cffi-type type)))))))

(defun section-text (features forms-groups missing)
  (with-standard-io-syntax
    (let ((*package* (ccl-vm-package))
          (*print-pretty* t)
          (*print-case* :downcase)
          (*print-right-margin* 110))
      (with-output-to-string (out)
        (format out "#+~a~%(progn" features)
        (loop for (forms separator) in forms-groups
              for first = t then nil
              do (unless first (terpri out))
                 (dolist (form forms) (format out "~a~s" separator form)))
        (format out ")~%")
        (when missing
          (format out "~%;; Not on this platform: ~{~a~^ ~}~%" missing))))))

(defparameter *new-file-header*
  ";;; The foreign types, records, constants and function signatures the VM uses, a section per
;;; platform.  Generated by tools/gen-ffi.lisp; don't edit.
;;;
;;; Nobody needs to regenerate this to use ccl-vm.  To add a platform, or update one, on that
;;; platform, with clang, in a lisp with Quicklisp, in the ccl-vm directory:
;;;   (load \"tools/gen-ffi.lisp\")
;;;   (ccl-vm-gen-ffi:gen-ffi \"ffi.lisp\")
;;; That replaces (or adds) the section for the machine it runs on, and leaves the others alone.
;;; A name added to tools/gen-ffi.lisp's lists needs every section regenerated.

(in-package :ccl-vm)
")

;; FILE's text before its first section, and its sections, each starting with its #+ line.
(defun read-sections (file)
  (if (not (probe-file file))
    (values *new-file-header*
            nil)
    (let ((lines (uiop:read-file-lines file))
          (header nil)
          (sections nil))
      (dolist (line lines)
        (if (uiop:string-prefix-p "#+" line)
          (push (list line) sections)
          (if sections (push line (first sections)) (push line header))))
      (flet ((text (lines) (format nil "~{~a~%~}" (reverse lines))))
        (values (text header) (mapcar #'text (reverse sections)))))))

(defun gen-ffi (file &key target sysroot (features (if target (error "Give :features with :target") (host-features))))
  (let* ((darwin (search "darwin" features))
         (info (c-info :headers (append *headers* (and darwin *darwin-headers*))
                       :target target :sysroot sysroot
                       :extra-args (and (not darwin) '("-D_GNU_SOURCE"))
                       :types *types* :records *records* :constants *constants* :functions *functions*))
         (missing (c-info-missing info)))
    (multiple-value-bind (type-forms record-forms constant-forms function-forms) (platform-definitions info)
      (let ((section (section-text features
                                   `((,type-forms ,(format nil "~%"))
                                     (,record-forms ,(format nil "~%~%"))
                                     (,constant-forms ,(format nil "~%"))
                                     (,function-forms ,(format nil "~%~%")))
                                   (remove-if-not (lambda (name) (member name missing :test #'string=))
                                                  (append *types* *records* *constants* *functions*))))
            (first-line (format nil "#+~a" features)))
        (multiple-value-bind (header sections) (read-sections file)
          (let ((old (position-if (lambda (text) (uiop:string-prefix-p (format nil "~a~%" first-line) text)) sections)))
            (if old
              (setf (nth old sections) section)
              (setq sections (append sections (list section)))))
          (with-open-file (out file :direction :output :if-exists :supersede)
            (format out "~a~{~%~%~a~}" (string-right-trim '(#\Newline) header)
                    (mapcar (lambda (text) (string-right-trim '(#\Newline) text)) sections))
            (terpri out)))))))
