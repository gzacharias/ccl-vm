(in-package :cl-user)

(defpackage :ccl-vm
  (:use :common-lisp)
  ;; (:import-from :split-sequence #:split-sequence)
  (:import-from :alexandria
                #:when-let
                #:when-let*
                #:if-let
                #:starts-with-subseq
                #:ends-with-subseq
                #:set-equal)
  (:export #:load-ccl
           #:cloop))

(in-package :ccl-vm)

;; True while the vm's own files are being loaded, and again while the .bc files are (see load-ccl).  It is defined here so
;; that it is already special when the files that mention it are compiled.  While it is true, the functions defined with
;; deflapfunction (runtime.lisp) are only queued, to be installed once the ccl packages exist.  loader.lisp sets it back to
;; nil at the end of the vm's load; after that a deflapfunction evaluated by itself installs right away.
(defvar *loading-ccl* t)
