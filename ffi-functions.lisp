;;; Signatures of the C functions the bundle calls with #_ (see def-external-call).  Maintained
;;; by hand: unlike the types, records and constants (tools/gen-ffi.lisp), they can't be found
;;; by a groveller.

(in-package :ccl-vm)

(def-external-call "memset" :pointer
  (ptr :pointer)
  (val :int)
  (size ccl-ffi::size_t))

(def-external-call "memmove" :pointer
  (dest :pointer)
  (src :pointer)
  (size ccl-ffi::size_t))

(def-external-call "getdtablesize" :int)

(def-external-call "nanosleep" :int
  (request :pointer)
  (remaining :pointer))

#+darwin
(def-external-call "mach_host_self" ccl-ffi::host_t)

#+darwin
(def-external-call "host_info" :int
  (host ccl-ffi::host_t)
  (flavor :int)
  (host-info-out :pointer)
  (host-info-out-cnt :pointer))

(def-external-call "getpagesize" :int)

(def-external-call "sysconf" :long
  (name :int))

(def-external-call "getuid" :int)

(def-external-call "getenv" :pointer
  (name :pointer))

(def-external-call "getpwuid_r" :int
  (uid :int)
  (pwd :pointer)
  (buffer :pointer)
  (size ccl-ffi::size_t)
  (result :pointer))

(def-external-call "uname" :int
  (buf :pointer))

(def-external-call "getcwd" :pointer
  (buf :pointer)
  (size ccl-ffi::size_t))

(def-external-call "isatty" :int
  (fd :int))

(def-external-call "chdir" :int
  (path :pointer))

(def-external-call "fpathconf" :long
  (fd :int)
  (size :int))

(def-external-call "getsockopt" :int
  (fd :int)
  (level :int)
  (option :int)
  (opt_value :pointer)
  (opt_len :pointer))

(def-external-call "rename" :int
  (old :pointer)
  (new :pointer))

(def-external-call "unlink" :int
  (path :pointer))

(def-external-call "mkdir" :int
  (path :pointer)
  (mode ccl-ffi::mode_t))

(def-external-call "rmdir" :int
  (path :pointer))

(def-external-call "strerror" :pointer
  (errno :int))

(def-external-call "getrusage" :int
  (who :int)
  (usage :pointer))
