(format t "~&;;;;;;;; ~a~%" (lisp-implementation-version))
(disassemble 'ccl::dll-header-length)
(format t "~&;;;;;;;; END~%")
(quit)
