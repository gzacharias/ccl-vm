;;; Loaded by run.sh ahead of each regression test file, natively or in the VM.
;;; (regression-check NAME FORM EXPECTED) evaluates FORM and EXPECTED and prints
;;;   REGRESSION PASS NAME                                 when the values are EQUAL, else
;;;   REGRESSION FAIL NAME: got <value>, expected <value>
;;; An error in FORM is its value: (:error <condition type> <message>), so it fails the check
;;; without stopping the file.  run.sh appends a form printing REGRESSION DONE, so a file that
;;; stops early (a reader error, an error outside a check) fails as well.

(defun report-regression-check (name form-thunk expected)
  (let ((actual (handler-case (funcall form-thunk)
                  (error (condition)
                    (list :error (type-of condition) (princ-to-string condition))))))
    (if (equal actual expected)
        (format t "~&REGRESSION PASS ~a~%" name)
        (format t "~&REGRESSION FAIL ~a: got ~s, expected ~s~%" name actual expected))))

(defmacro regression-check (name form expected)
  `(report-regression-check ',name (lambda () ,form) ,expected))
