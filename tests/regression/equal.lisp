;;; EQUAL of equal non-immediate numbers (bignums, ratios, double-floats) is true.
;;; The VM's EQUAL called HAIRY-EQUAL on uvectors without trying EQL first, as native EQUAL does,
;;; and HAIRY-EQUAL has no case for numbers.
;;; Guards: ccl-vm 59e0aad (EQUAL tries LAP-EQL before HAIRY-EQUAL).
;;; Not regression-check's own EQUAL: these compare with EQ on the result.

(regression-check bignum (eq (equal (expt 2 70) (identity (expt 2 70))) t) t)
(regression-check ratio (eq (equal 1/3 (/ 1 (identity 3))) t) t)
(regression-check double-float (eq (equal 1.5d0 (+ (identity 1d0) 0.5d0)) t) t)
(regression-check in-list (eq (equal (list (expt 2 70) 1/3) (list (expt 2 70) (/ 1 (identity 3)))) t) t)
(regression-check different-bignums (equal (expt 2 70) (identity (expt 2 71))) nil)
;; EQUAL is EQL on numbers, so no type contagion and signed zeros differ.
(regression-check no-contagion (equal 1d0 (identity 1)) nil)
(regression-check signed-zeros (equal 0d0 (- (identity 0d0))) nil)
