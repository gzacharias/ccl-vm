(in-package "CCL")
(let ((f (arch::target-array-data-size-function (backend-target-arch *host-backend*))))
  (format t "~&RESULT ~a ~s~%" (lisp-implementation-version) f)
  (dolist (bogus '(foo (a b) 1.5 "x" #\a))
    (format t "~&RESULT   subtag ~s -> ~a~%" bogus
            (handler-case (prin1-to-string (funcall f bogus 10))
              (error (c) (format nil "ERROR: ~a" c))))))
(quit)
