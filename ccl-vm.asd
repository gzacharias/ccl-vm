;;;; -*- Mode: Lisp -*-
;;;; ASDF system definition for ccl-vm: a virtual machine, written in portable Common Lisp, that runs Clozure CL.
;;;;

(asdf:defsystem "ccl-vm"
  :description "A virtual machine that runs Clozure CL from .bc files inside another Common Lisp"
  :license "Apache-2.0"
  :depends-on ("cffi" "alexandria")
  :serial t
  :components ((:file "package")
               (:file "defs")
               (:file "types")
               (:file "syms")
               (:file "funcs")
               (:file "bceval")
               (:file "runtime")
               (:file "loader")))
