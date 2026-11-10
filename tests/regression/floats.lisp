;;; Float negation, signed zeros, NaNs and arithmetic errors.
;;; Guards: ccl-vm f4e6e22 (float fixes), 2719ec2 (host arithmetic errors are signalled as the
;;; same-named CCL conditions, ccl-vm#2).  Before 97d1eef the VM output was diffed against the
;;; native one; these expected values are what native, sbcl and ccl all printed on 2026-11-09.
;;; (identity ...) keeps the compiler from folding the operation.

(defun float-bits (x) (multiple-value-list (ccl::double-float-bits x)))

(regression-check neg0 (float-bits (- (identity 0d0))) '(#x80000000 0))
(regression-check negm0 (float-bits (- (identity -0d0))) '(0 0))
(regression-check negnan (float-bits (- (identity ccl::double-float-nan))) '(#xfff80000 0))
(regression-check mulnegnan (float-bits (* 2d0 (- (identity ccl::double-float-nan)))) '(#xfff80000 0))
(regression-check addnan (float-bits (+ (identity ccl::double-float-nan) 0d0)) '(#x7ff80000 0))
(regression-check funcall-negate (float-bits (funcall 'ccl::%negate (identity ccl::double-float-nan))) '(#xfff80000 0))
(regression-check decl-neg
  (float-bits (funcall (compile nil '(lambda (x) (declare (double-float x)) (- x))) 0d0))
  '(#x80000000 0))
(regression-check sdecl-neg
  (funcall (compile nil '(lambda (x) (declare (single-float x)) (- x))) 0.0)
  -0.0)
(regression-check single
  (list -0.0 (- (identity 0.0)) (- (identity 1.5)) (list (float-sign -0.0) (abs -0.0) (minusp -0.0)))
  '(-0.0 -0.0 -1.5 (-1.0 0.0 nil)))
(regression-check double-print
  (list (prin1-to-string -0d0) (prin1-to-string (- (identity 0d0))))
  '("-0.0D0" "-0.0D0"))
(regression-check inf (float-bits (+ (identity ccl::double-float-positive-infinity) 1d0)) '(#x7ff00000 0))
(regression-check sub0 (float-bits (- 0 (identity 0d0))) '(0 0))
(regression-check fix
  (list (- (identity 5)) (- (identity most-negative-fixnum)))
  (list -5 (1+ most-positive-fixnum)))
(regression-check ratbig
  (list (- (identity 1/3)) (- (identity (expt 2 70))))
  (list -1/3 (- (expt 2 70))))

;; Arithmetic errors are CCL conditions that CCL code can handle (in the VM, translated from the
;; host's).  Any other error fails the check with its type.
(regression-check arith-div0
  (handler-case (/ (identity 1) (identity 0)) (division-by-zero () :caught))
  :caught)
(regression-check arith-nan=
  (handler-case (= (identity ccl::double-float-nan) (identity ccl::double-float-nan))
    (floating-point-invalid-operation () :caught))
  :caught)
(regression-check arith-nan<
  (handler-case (< (identity ccl::double-float-nan) 1d0) (floating-point-invalid-operation () :caught))
  :caught)
(regression-check arith-overflow
  (handler-case (* (identity most-positive-double-float) 10d0) (floating-point-overflow () :caught))
  :caught)
