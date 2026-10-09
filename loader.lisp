(in-package :ccl-vm)

;; Maybe should load nfasload and just comment out %fasload?  I think there is a mechanism for
;; different fasload backends already!

(defparameter *loading-ccl* nil)

(defun cloop ()
  (let ((*package* *native-package*)) ;; for debugging
    (loop
      (restart-case (return (ccl-funcall *ccl-toplevel-func*))
        (restart-cloop () :report (lambda (s) (format s "Restart CVM toplevel")))))))

;;; ~5 mins
(defun load-ccl (bc-bundle &key ccl-directory)
  (unless (probe-file (merge-pathnames "level-1.bc" bc-bundle))
    (error "~a doesn't look like a ccl bc directory, it has no level-1.bc" bc-bundle))
  (when (and ccl-directory (not (probe-file (merge-pathnames "level-1" ccl-directory))))
    (error "~a doesn't look like ccl directory" ccl-directory))

  (setq bc-bundle (truename bc-bundle))

  (let ((*loading-ccl* t))
    (init-packages)
    (init-lap-functions)
    ;;; CMAIN is a nilreg-relative symbol, so is accessible in the kernel.
    ;;;  It gets set to XCMAIN which each architecture is supposed to define as a CALLBACK, i.e.
    ;;;  a way for kernel to call us.   It seems to be only called for signals.
    (%set-sym-value (ccl-symbol 'xcmain) 'callback-for-cmain?)
    (%set-sym-value (ccl-symbol '%xerr-disp) 'callback-for-%err-disp?)

    (init-fake-addresses)

    ;; This is only used to set the CCL: logical name when not found by getenv. It must be a file that exists at toplevel
    ;; in the ccl directory.
    (when *fake-heap-image-name* ;; clear from previous runs
      (cffi:foreign-string-free *fake-heap-image-name*))
    (setq *fake-heap-image-name*
          (cffi:foreign-string-alloc 
           (namestring (make-pathname :name "level-1" :type "bc" :defaults bc-bundle))))

    (setf (sym-value (ccl-symbol '*gf-proto*)) (sym-func (ccl 'gag-any-arg)))
    
    (let ((lock (make-rw-lock-obj)))
      (setf (sym-value (ccl-symbol '%all-packages-lock%)) lock)
      (setf (sym-value (ccl-symbol '%system-locks%)) (make-uvector subtag-population (vector 0 0 (gvref lock 0)))))
    

    (let ((*default-pathname-defaults* bc-bundle))
      (cvm-load-level-0)
    ;; Ok, so this sets toplevel function at the end then throws to toplevel...
    ;; The toplevel func basically calls #'toplevel-loop
      (catch (ccl-symbol :toplevel)
        (let ((*load-verbose* t))
          (lap-%fasload (sym-value (ccl-symbol '*xload-startup-file*)))))))

  ;; The boot is over.  Stop redirecting REQUIRE's to the bc bundle.
  (ccl-funcall (sym-func (ccl 'forget-boot-search-path)))
  ;; Set alternate "ccl:" if requested
  (when ccl-directory
    (ccl-funcall (sym-func (ccl 'set-ccl-directory)) 
                 (ccl (namestring (translate-logical-pathname ccl-directory)))))


  (setf (sym-value (ccl'*listener-prompt-format*)) (ccl "~[ccl?~:;~:*ccl ~d >~] "))
  #+ccl (clear-input ccl::*stdin*) ;; for some reason, needed when restarting after errors when using AltConsole 
  (format t "~&CCL-VM LOADED, now can ~s" '(ccl-funcall *ccl-toplevel-func*)))


;; Build things up to the point where in the bootstrapping version, the heap image
;; has been loaded and all the initializations in %toplevel-function% in nfasload
;; have been executed up.
(defun cvm-load-level-0 ()
  (let* ((files (sort (directory "level-0/**/*.bc") ;; TODO: get rid of "bc"? *.fasl-pathname* not defined til l1-files. use *xload-startup-file*?
                      #'string-lessp :key #'pathname-name))
         (calls (loop for file in files
                  unless (string-equal (pathname-name file) "nfasload")
                  nconc (let ((*deferred-level-0-calls* (list t)))
                          (cvmload file)
                          (loop for call in (cdr (nreverse *deferred-level-0-calls*))
                            collect (list file call))))))
    ;; Some stuff xfasload inits at image-build time
    ;; Most of this could be done before loading level-0, once packages exist.
    (%defvar (ccl-symbol '*package*) () 'variable *ccl-pkg*)
    (%defvar (ccl-symbol '*ccl-package*) () 'variable *ccl-pkg*)
    (%defvar (ccl '*common-lisp-package*) () 'variable *cl-pkg*)
    (%defconstant (ccl '%unbound-function%) *unbound-function*)
    (%defvar (ccl '%builtin-functions%) () 'variable
             (make-uvector subtag-simple-vector
                           (map 'vector #'ccl-symbol 
                                #(+-2 --2 *-2 /-2 =-2 /=-2 >-2 >=-2 <-2 <=-2 eql length sequence-type
                                      assq memq logbitp logior-2 logand-2 ash 
                                      %negate logxor-2 %aref1 %aset1))))

    (%defvar (ccl '*keyword-package*) () 'variable *keyword-pkg*)
    (%defvar (ccl'*gc-event-status-bits*) () 'variable 0)
    (%defvar (ccl '%toplevel-catch%) () 'variable (ccl :toplevel))
    ; %closure-code%, %macro-code%, %builtin-functions%
    ;; Macros sym-func is a vector #(<macro-code> fn)
    (%defvar (ccl '%macro-code%) () 'variable *macro-apply-code*)
    ;;(setf (xload-symbol-value (xload-copy-symbol '*xload-cold-load-documentation*))
    ;;      (xload-save-list (setq *xload-cold-load-documentation*
    ;;                             (nreverse *xload-cold-load-documentation*))))
    
    ;; The "cold load" stream.
    (loop for (file fn) in calls as index upfrom 1
      do (format t "~& Call #~s (from ~s) " index file)
      do (ccl-funcall fn))

    ;;;; TODO******* So this needs to somehow come in from the compiler, because that's who knowns where it puts it.
    (%defvar (ccl '*xload-startup-file*) () 'variable (ccl "level-1.bc"))
    (%defvar (ccl '*openmcl-svn-revision*) () 'variable nil) ;; (local-vc-revision) -- SO THIS NEEDS TO BE FROM COMPILE/XLOAD time again
    (%defvar (ccl '*optional-features*) () 'variable nil) ;(mapcar 'ccl-symbol CCL::*BUILD-TIME-OPTIONAL-FEATURES*)

    (unbootstrap-documentation)
    ;;(unbootstrap-packages)
    ;;(%fasload *xload-startup-file*))
    (format t "~&Level-0 loaded~%")))

    

;;  When running in the VM, bc files need to be recognized as fasl files,
;;  so our {%fasload} function can run and do the load using cvmload.  This is
;;  accomplished by loading the cvm backend into the the VM, which makes {fasl-file-p}
;;  be true for bc files.

(defun cvmload  (file)
  (assert (equal (pathname-type file) "bc"))
  ;; Should we compile then load?  Only worth if can avoid the compile!
  ;; Which means we need to figure out fasl file conventions in the lisp.
  ;; Worry about it later
  (loop
    (restart-case ;; remove this once debugged
        (return (let ((*loader-table* nil)
                      (*package* (find-package :ccl-vm))
                      (cur-pkg (%sym-value (ccl'*package*)))
                      (cur-rdtable (%sym-value (ccl'*readable*))))
                  (declare (special *loader-table*))
                  (unwind-protect
                      ;; We want to load this as a source file. There is no way to ensure that portably,
                      ;; but in practice any lisp would interpret a random text file as source, except for
                      ;; this little weirdness in CCL that we introduced...
                      (let (#+ccl(ccl::*known-backends* (remove (pathname-type file) ccl::*known-backends*
                                                                :key (lambda (b)
                                                                       (pathname-type (ccl::backend-target-fasl-pathname b)))
                                                                :test 'equal)))
                        (load file))
                    (%set-sym-value (ccl'*package*) cur-pkg)
                    (%set-sym-value (ccl'*readable*) cur-rdtable) cur-rdtable)))
      (retry-load () :report (lambda (s) (format s "CVMLOAD ~s again" file))))))


;; a BC file is a bunch of toplevel calls to these $fasl functions.  The arguments
;; (once evaluated in the host lisp) are BC expressions, can then be bceval'ed to yield
;; various native objects, or effect sideffects in the VM...

(defvar *fasl-trace* nil)
(defmacro fasl-trace (&rest format-args)
  `(when *fasl-trace*
     (let ((*print-pretty* t)
           (*print-circle* nil))
       (fresh-line *trace-output*)
       (format *trace-output* ,@format-args))))


(defun $fasl-init (num-imms)
  (declare (special *loader-table*))
  (assert (boundp '*loader-table*))
  (assert (null *loader-table*))
  (setq *loader-table* (make-array num-imms)))

(defun $fasl-set-package (str)
  (fasl-trace "~s ~s" '$fasl-set-package str)
  (check-type str ccl-simple-string)
  (let ((pkg (pkg-arg str)))
    ;;(assert (eq pkg *ccl-pkg*))
    (setf (sym-value (ccl '*package*)) pkg)))

(defun $fasl-defvar (sym &optional doc)
  (fasl-trace "~s ~s ~s" '$fasl-defvar sym doc)
  (%defvar sym doc 'variable))

(defun $fasl-defparameter (sym val doc)
  (fasl-trace "~s ~s ~s ~s" '$fasl-defparameter sym val doc)
  (%defvar sym doc 'variable)
  (setf (sym-value sym) val))

(defun $fasl-defvar-init (sym val doc)
  (fasl-trace "~s ~s ~s ~s" '$fasl-defvar-init sym val doc)
  (%defvar sym doc 'defvar)
  (unless (sym-boundp sym)
    (setf (sym-value sym) val)))


;; boostrapping until L1-utils
(defun $fasl-defconstant (sym val doc)
  (fasl-trace "~s ~s ~s ~s" '$fasl-constant sym val doc)
  ;; once bootstrapped, this will check for redefinition.  Not here.
  (%defconstant sym val doc))

(defun $fasl-defun (fn &optional doc)
  (fasl-trace "~s ~s ~s" '$fasl-defun fn doc)
  (%defun fn doc))

(defun $fasl-funcall (sym-or-fn)
  (fasl-trace "~s ~s" '$fasl-funcall sym-or-fn)
  (let ((fn (ensure-func sym-or-fn)))
    (if *deferred-level-0-calls*
      (push fn *deferred-level-0-calls*)
      (ccl-funcall fn))))

(defun $fasl-defmacro (fn doc)
  (fasl-trace "~s ~s ~s" '$fasl-defmacro fn doc)
  (check-type fn ccl-function)
  (let ((sym (ccl-function-name fn)))
    (check-type sym ccl-symbol)
    (record-debug-info sym doc 'function)
    (ccl-set-macro-function sym fn)))

(defun $fs-ref (index)
  (declare (special *loader-table*))
  (svref *loader-table* index))

(defun $fs-set (index value)
  (declare (special *loader-table*))
  (setf (svref *loader-table* index) value))

(defun $fs-unbound-marker () *unbound-marker*)
(defun $fs-slot-unbound-marker () *slot-unbound-marker*)
(defun $fs-illegal-marker () *illegal-marker*)
(defun $fs-unbound-function () *unbound-function*)

(defun $fs-char (code) (code-char code)) ;; for non-standard chars

(defun $fs-package (name)
  (fasl-trace "   ~s ~s" '$fs-package name)
  (check-type name ccl-simple-string)
  (pkg-arg name))

(defun $fs-symbol (name pkg)
  (fasl-trace "   ~s ~s ~s" '$fs-symbol name pkg)
  (if (null pkg)
    (make-ccl-symvector name)
    (find-or-make-sym name pkg)))

(defun $fs-string (string)
  (check-type string string)
  (ccl-string string))

(defun $fs-make-uvector (type-key size)
  (fasl-trace "   ~s ~s ~s" '$fs-make-uvector type-key size)
  (check-type size fixnum)
  (alloc-uvector size (typekey-subtag type-key)))

(defun $fs-cons-function ()
  (fasl-trace "   ~s" '$fs-cons-function)
  (cons-ccl-function))

(defun $fs-init-function (fn bclambda)
  (let ((*print-length* 3) (*print-level* 3))
  (fasl-trace "   ~s ~s ~s" '$fs-init-function fn bclambda))
  (init-ccl-function fn bclambda))

(defun $fs-init-uvector (uvec &rest values)
  (fasl-trace "   ~s ~s ~s" '$fs-init-uvector uvec values)
  (assert (eq (uvsize uvec) (length values)))
  (loop for val in values as index upfrom 0
    do (setf (uvref uvec index) (ccl val)))
  uvec)

(defun $fs-make-array (native-key dims-list)
  (fasl-trace "   ~s ~s ~s" '$fs-make-array native-key dims-list)
  (break "NIY")
  'make-array-not-implemented-yet)

(defun $fs-init-array (arr &rest row-major-values)
  (fasl-trace "   ~s ~s ~s" '$fs-init-array arr row-major-values)
  (break "NIY")
  'init-array-not-implemented-yet)

(defun $fs-eval (expr)
  (fasl-trace "   ~s ~s" '$fs-eval expr)
  (when *deferred-level-0-calls*
    (error "$fs-eval in level-0 ~s" expr))
  (labels ((simple-eval (arg)
           (cond ((typep arg 'ccl-symvector) (sym-value arg))
                 ((atom arg) arg)
                 ((eq (car arg) (ccl'quote))
                  (assert (eql (length arg) 2))
                  (cadr arg))
                 ((typep (car arg) 'ccl-symvector)
                  ;; this will err out on macros or special forms
                  (apply-in-environment nil (car arg) (mapcar #'simple-eval (cdr arg))))
                 (t (error "Don't know how to eval ~s" expr)))))
    (simple-eval expr)))

;; like $fasl-funcall but for value, it's used in load-time values.
;; called for - register-package-ref, find-builtin-cell, register-type-cell, find-class-cell,
;; ensure-slot-id, specifier-type  Mainly find-class-cell.  There are also multiple calls with the same
;; args.  Might avoid that if caught it at at compile time, like $fs-istruct-cell.
(defun $fs-funcall (fn)
  (fasl-trace "   ~s ~s" '$fs-funcall fn)
  (when *deferred-level-0-calls*
    (error "$fs-funcall in level-0 ~s" fn))
  (ccl-funcall fn))

(defun $fs-istruct-cell (sym)
  (fasl-trace "   ~s ~s" '$fs-istruct-cell sym)
  (check-type sym ccl-symbol)
  (register-istruct-cell sym))
