(in-package :ccl-vm)

(defvar *CCL-DIRECTORY*)

(defmacro defun-inline (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))

(defmacro defvar-typed (var type)
  `(progn
     (declaim (type ,type ,var))
     (defvar ,var)))

(defun-inline fixnump (x) (typep x 'fixnum))

;;; This must match cvm-arch.  Figure out some way to share

;; A lot of the front end of the compiler, and some random ccl code, assumes a certain basic
;; architecture in terms of what types have their own tags, etc. so stick close to that.

(defconstant fulltag-even-fixnum 0)
(defconstant fulltag-single-float 1)
(defconstant fulltag-character 2)
(defconstant fulltag-cons 3)
(defconstant fulltag-nil 11)
(defconstant fulltag-immediate 4) ;; was tra-0, reuse it...
(defconstant fulltag-odd-fixnum 8)
;; 12 is available (was tra-1)
(defconstant fulltag-misc 13)
(defconstant fulltag-symbol 14)
(defconstant fulltag-function 15)

(defconstant lisptag-fixnum 0)
(defconstant lisptag-single-float 1)
(defconstant lisptag-character 2)
(defconstant lisptag-list 3)
(defconstant lisptag-immediate 4)
(defconstant lisptag-misc 5)
(defconstant lisptag-symbol 6)
(defconstant lisptag-function 7)

;; Pass 1 of the compiler assumes this, so we have no choice.  See *nx-64-bit-fixnum-type*
(defconstant num-fixnum-bits 61)
(defconstant fixnum-shift (- 64  num-fixnum-bits))
(defconstant full-fixnum-mask (lognot (ash -1 num-fixnum-bits))) ;;  with sign bit, not a fixnum
(defconstant unsigned-fixnum-mask (ash full-fixnum-mask -1)) ;; without sign bit, just the data

(defconstant IEEE-single-float-digits 24)
(defconstant IEEE-double-float-digits 53)
;; We want to be able to represent single floats as native single floats
(assert (>= (float-digits 1.0s0) IEEE-single-float-digits))



;; These values never occur as fulltags, so can be used for misc vector subtags without confusion
(defconstant gvector-subtags-0 5)
(defconstant gvector-subtags-1 6)
(defconstant ivector-subtags-misc 7)
(defconstant ivector-subtags-32-bit 9)
(defconstant ivector-subtags-64-bit 10)

(defparameter *uvector-subtag-typekeys* (make-array 256 :initial-element nil))

(defmacro subtag-typekey (subtag)
  `(or (svref *uvector-subtag-typekeys* ,subtag) (error "Unknown subtag")))

(defmacro typekey-subtag (typekey)
  `(or (position ,typekey *uvector-subtag-typekeys*) (error "Unknown typekey")))


(defun gvector-type-p (subtag-or-typekey)
  (let* ((subtag (if (fixnump subtag-or-typekey)
                   subtag-or-typekey
                   (typekey-subtag subtag-or-typekey)))
         (tag (logand subtag #xF)))
    (when (or (eq tag gvector-subtags-0) (eq tag gvector-subtags-1)) subtag)))

(defun ivector-type-p (subtag-or-typekey)
  (let* ((subtag (if (fixnump subtag-or-typekey)
                   subtag-or-typekey
                   (typekey-subtag subtag-or-typekey)))
         (tag (logand subtag #xF)))
    (when (or (eq tag ivector-subtags-misc)
              (eq tag ivector-subtags-32-bit)
              (eq tag ivector-subtags-64-bit))
      subtag)))


#+hemlock (hemlock::defindent "define-subtags" 1)
(defmacro define-subtags (code &rest names)
  `(progn
     ,@(loop for index = #x10 then (+ index #x10)
         for spec in names
         as name = (if (consp spec) (car spec) spec)
         as key = (let ((pname (string name)))
                    (assert (string= "SUBTAG-" pname :end2 (length "SUBTAG-")))
                    (intern (subseq pname (length "SUBTAG-")) :keyword))
         do (when (consp spec)
              (let ((new-index (ash (cadr spec) 4)))
                (assert (<= index new-index))
                (setq index new-index)))
         do (assert (<= index #xF00))
         collect `(defconstant ,name (+ ,code ,index))
         collect `(setf (svref *uvector-subtag-typekeys* ,name) ,key))))

(define-subtags gvector-subtags-0
  subtag-symvector      ;; ccl-symvector
  subtag-catch-frame
  subtag-hash-vector
  subtag-pool
  subtag-population
  subtag-package     ;; ccl-package
  subtag-slot-vector
  subtag-basic-stream
  subtag-function    ;; ccl-function
  subtag-call-frame ;; Just for us!
  (subtag-array-header 11))

(define-subtags gvector-subtags-1
  subtag-ratio
  subtag-complex
  subtag-struct      ;; ccl-struct
  subtag-istruct     ;; ccl-istruct
  subtag-value-cell
  subtag-xfunction
  subtag-lock
  subtag-instance    ;; ccl-instance
  subtag-lexpr-vector   ;; Just for us!
  (subtag-vector-header 11)
  subtag-simple-vector)

(defconstant min-cl-ivector-subtag #x90) ;; CL ivector subtags start at 9

(define-subtags ivector-subtags-misc
  ;; common lisp vectors
  (subtag-complex-double-float-vector 9)
  subtag-signed-16-bit-vector 
  subtag-unsigned-16-bit-vector
  (subtag-signed-8-bit-vector 13)
  subtag-unsigned-8-bit-vector
  subtag-bit-vector)

(define-subtags ivector-subtags-32-bit
  subtag-bignum              ;; ccl-bignum
  subtag-double-float
  subtag-xcode-vector
  subtag-complex-single-float
  subtag-complex-double-float
  ;; common lisp vectors
  (subtag-simple-string 12)  ;; ccl-simple-string
  subtag-signed-32-bit-vector
  subtag-unsigned-32-bit-vector
  subtag-single-float-vector)

(define-subtags ivector-subtags-64-bit
  subtag-macptr               ;; ccl-macptr
  subtag-dead-macptr
  ;; Common lisp vectors)
  (subtag-complex-single-float-vector 11)
  subtag-fixnum-vector
  subtag-signed-64-bit-vector
  subtag-unsigned-64-bit-vector
  subtag-double-float-vector)

(declaim (type simple-vector *subtag-ffi-types*))
(defparameter *subtag-ffi-types*
  (let ((arr (make-array 256 :initial-element nil)))
    (setf (svref arr subtag-bit-vector) :bit)
    (setf (svref arr subtag-signed-8-bit-vector) :int8)
    (setf (svref arr subtag-unsigned-8-bit-vector) :uint8)
    (setf (svref arr subtag-signed-16-bit-vector) :int16)
    (setf (svref arr subtag-unsigned-16-bit-vector) :uint16)
    (setf (svref arr subtag-signed-32-bit-vector) :int32)
    (setf (svref arr subtag-unsigned-32-bit-vector) :uint32)
    (setf (svref arr subtag-signed-64-bit-vector) :int64)
    (setf (svref arr subtag-unsigned-64-bit-vector) :uint64)
    (setf (svref arr subtag-complex-double-float-vector) '(:array 4 :uint32))
    (setf (svref arr subtag-bignum) :uint32)
    (setf (svref arr subtag-double-float) :uint32)
    (loop for i from ivector-subtags-32-bit below 256 by #x10
      do (when (svref *uvector-subtag-typekeys* i)
           (setf (svref arr i) :uint32)))
    (setf (svref arr subtag-signed-32-bit-vector) :int32)
    (loop for i from ivector-subtags-64-bit below 256 by #x10
      do (when (svref *uvector-subtag-typekeys* i)
           (setf (svref arr i) :uint64)))
    (setf (svref arr subtag-fixnum-vector) :int64)
    (setf (svref arr subtag-signed-64-bit-vector) :int64)
    arr))

(defconstant numeric-subtag-mask
  (logior (ash 1 fulltag-even-fixnum)
          (ash 1 fulltag-odd-fixnum)
          (ash 1 subtag-bignum)
          (ash 1 subtag-ratio)
          (ash 1 fulltag-single-float)
          (ash 1 subtag-double-float)
          (ash 1 subtag-complex)
          (ash 1 subtag-complex-single-float)
          (ash 1 subtag-complex-double-float)))


;(defconstant $flags_Normal 0)
;(defconstant $flags_DisposeRecursiveLock 1)
;(defconstant $flags_DisposPtr 2)
(defconstant $flags_DisposeRwlock 3)
;(defconstant $flags_DisposeSemaphore 4)

;(defconstant $system-lock-type-recursive 0)
;(defconstant $system-lock-type-rwlock 1)


;; host symbols are not accessible from the VM, so can use them as unique values.
(defparameter *unbound-marker* 'unbound-marker)
(defparameter *slot-unbound-marker* 'slot-unbound-marker)
(defparameter *illegal-marker* 'illegal-marker)

(defparameter *unbound-function* 'unbound-function)
(defparameter *macro-apply-code* 'macro-apply-code)

;; errors
(defconstant $xnofinfunction 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; random utils

(defun-inline require-type (obj type)
  (if (typep obj type) obj (require-type-out-of-line obj type)))

(defun require-type-out-of-line (obj type)
  (assert (typep obj type) (obj))
  obj)

(defun report-bad-arg (obj type)
  (error "The value ~s is not of the expected type ~s" obj type))

(defmacro cassert (form)
  `(unless ,form (cerror "Ignore it" "assert failed ~s" ',form)))

#+hemlock (hemlock::defindent "named-function" 2)
(defmacro named-function (name arglist &body body)
  (declare (ignorable name))
  (when (and (null body) (eq (car arglist) 'lambda))
    (setq body (cddr arglist) arglist (cadr arglist)))
  #+ccl `(ccl:nfunction ,name (lambda ,arglist ,@body))
  #-ccl `(function (lambda ,arglist ,@body)))

(defmacro without-fpu-overflow (&body body)
  #+ccl `(let ((overflow (ccl:get-fpu-mode :overflow)))
           (unwind-protect
               (progn
                 (when overflow (ccl:set-fpu-mode :overflow nil))
                 ,@body)
             (when overflow (ccl:set-fpu-mode :overflow overflow))))
  #+sbcl `(let* ((traps (getf (sb-int:get-floating-point-modes) :traps))
                 (overflow (member :overflow traps)))
            (unwind-protect
                (progn
                  (when overflow
                    (sb-int:set-floating-point-modes :traps (remove :overflow traps)))
                  ,@body)
              (when overflow (sb-int:set-floating-point-modes :traps traps))))
  #-(or ccl sbcl) `(handler-case  (progn ,@body)
                    (floating-point-overflow () (error "Need to implement WITHOUT-FPU-OVERFLOW"))))


