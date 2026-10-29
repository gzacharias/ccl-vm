;;; Loaded by the vm script into the host lisp: loads ccl-vm from the tree this file is in.
#+sbcl (setf (sb-ext:bytes-consed-between-gcs) (* 1024 1024 1024))
(load "~/quicklisp/setup.lisp")
(pushnew (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*))
         asdf:*central-registry* :test #'equal)
(asdf:load-system "ccl-vm")
