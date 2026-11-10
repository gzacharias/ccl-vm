;;; double-float-nan: the same value on all hosts, a real constant, and define-constant accepting
;;; a redefinition with a NaN of the same bits.
;;; Guards: ccl bc-compiler 989a3ce9 (double-float-nan has the same value on all hosts) and the
;;; define-constant PR (ccl PR-define-constant).  Expected values: what native, sbcl and ccl all
;;; printed on 2026-11-09.

(defun nan-bits (x) (multiple-value-list (ccl::double-float-bits x)))

(regression-check nan-bits (nan-bits ccl::double-float-nan) '(#x7ff80000 0))
(regression-check nan-constantp-boundp
  (list (constantp 'ccl::double-float-nan) (boundp 'ccl::double-float-nan))
  '(t t))
(regression-check compiled-ref
  (nan-bits (funcall (compile nil '(lambda () ccl::double-float-nan))))
  '(#x7ff80000 0))
(regression-check parse-nan (nan-bits (read-from-string "1.0d+-0")) '(#x7ff80000 0))
(regression-check gensyms-per-compile
  (let ((before *gensym-counter*)) (compile nil '(lambda (x) (car x))) (- *gensym-counter* before))
  0)
;; Fresh NaNs by negation (the VM has no double-float-from-bits): (- (- nan)) has
;; double-float-nan's bits (#x7ff80000 0), (- nan) the sign bit flipped (#xfff80000 0).
;; Redefining with the same bits is no redefinition, even when cerroring is on; a NaN with
;; different bits is a redefinition, allowed when cerroring is off.
(regression-check define-constant-nan
  (flet ((fresh-nan () (- (- (identity ccl::double-float-nan)))))
    (let ((nan1 (fresh-nan))
          (nan2 (- (identity ccl::double-float-nan)))
          (constant-name (gensym)))
      (ccl::define-constant constant-name nan1)
      (list (let ((ccl::*cerror-on-constant-redefinition* t))
              (ccl::define-constant constant-name (fresh-nan))
              (eq (symbol-value constant-name) nan1))
            (let ((ccl::*cerror-on-constant-redefinition* nil))
              (ccl::define-constant constant-name nan2)
              (eq (symbol-value constant-name) nan2)))))
  '(t t))
