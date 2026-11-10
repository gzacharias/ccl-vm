;;; Prints CHECK lines about double-float-nan handling (bc-compiler 989a3ce9 and the define-constant
;;; PR); run natively or in the VM by run-checks.sh.  Ends with CHECK-DONE.
(progn
  (format t "~&CHECK NAN-BITS ~x~%" (ignore-errors (multiple-value-list (ccl::double-float-bits ccl::double-float-nan))))
  (format t "~&CHECK NAN-CONSTANTP ~s BOUNDP ~s~%" (constantp 'ccl::double-float-nan) (boundp 'ccl::double-float-nan))
  (format t "~&CHECK COMPILED-REF ~x~%"
          (handler-case (multiple-value-list (ccl::double-float-bits (funcall (compile nil '(lambda () ccl::double-float-nan)))))
            (error (c) (format nil "ERROR ~a" c))))
  (format t "~&CHECK PARSE-NAN ~x~%"
          (handler-case (multiple-value-list (ccl::double-float-bits (read-from-string "1.0d+-0")))
            (error (c) (format nil "ERROR ~a" c))))
  (format t "~&CHECK GENSYMS-PER-COMPILE ~s~%"
          (let ((before *gensym-counter*)) (compile nil '(lambda (x) (car x))) (- *gensym-counter* before)))
  (format t "~&CHECK DEFINE-CONSTANT-NAN ~s~%"
          (let ((nan1 (ccl::double-float-from-bits #x7ff80000 0))
                (nan2 (ccl::double-float-from-bits #xfff80000 0))
                (sym (gensym)))
            (ccl::define-constant sym nan1)
            (list (let ((ccl::*cerror-on-constant-redefinition* t))
                    (handler-case (progn (ccl::define-constant sym (ccl::double-float-from-bits #x7ff80000 0))
                                         (eq (symbol-value sym) nan1))
                      (error (c) (type-of c))))
                  (let ((ccl::*cerror-on-constant-redefinition* nil))
                    (handler-case (progn (ccl::define-constant sym nan2) (eq (symbol-value sym) nan2))
                      (error (c) (type-of c)))))))
  (format t "~&CHECK-DONE~%"))
