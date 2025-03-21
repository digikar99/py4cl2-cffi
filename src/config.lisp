(defpackage :py4cl2-cffi/config
  (:use :cl)
  (:export #:+python-version-string+
           #:*python-ldflags*
           #:*python-ignore-ldflags*
           #:*python-includes*
           #:*python-compile-command*
           #:*python-numpy-compile-command*
           #:*python-executable-path*
           #:*python-site-packages-path*
           #:*disable-pystop*
           #:*python-call-mode*
           #:print-configuration
           #:shared-library-from-ldflag))

(in-package :py4cl2-cffi/config)

;; Basic principle: We aim to use python3-config or equivalent to discover these values
;; However, whenever there is a problem with getting the python-executable path, then we
;; set it *explicitly* externally using an environment variable, e.g., via including
;; something like
;;
;; (setf (uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH")	
;;      "/usr/local/Caskroom/miniconda/base/bin/python3")
;;
;; into .sbclrc
;;
;; Then, we use that path (if set) for inferring the other values

(declaim (type list
               *python-ldflags*
               *python-includes*))

(declaim (type string *python-compile-command*))

(defvar *python-compile-command*
  (concatenate
   'string
   "gcc ~A -c -Wall -Werror -fpic py4cl-utils.c && "
   "gcc -shared -o libpy4cl-utils.so py4cl-utils.o")
  "~A corresponds to the *python-includes*
")

(defvar *python-numpy-compile-command*
  (concatenate
   'string
   "gcc ~A -I'~A' -c -Wall -Werror -fpic py4cl-numpy-utils.c && "
   "gcc -shared -o libpy4cl-numpy-utils.so py4cl-numpy-utils.o")
  "The first~A corresponds to the *python-includes*
The second ~A corresponds to the numpy include files discovered
  in shared-objects.lisp
")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun return-value-as-list (cmd)
    (remove ""
            (mapcar (lambda (value)
                      (string-trim '(#\newline) value))
                    (uiop:split-string
                     (uiop:run-program cmd
                                       :output :string
                                       :error-output *error-output*)
                     :separator '(#\newline #\tab #\space)))
            :test #'string=))
  
  (defvar *python-executable-path*
    (if (uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH")
	(uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH")
	(first (return-value-as-list "which python3")))
    "The path to python executable. This will be used to set sys.path.
This is useful in cases such as venv when python3-config does not lead
to expected paths.")

  (alexandria:define-constant +python-version-string+
      (second (return-value-as-list (format nil "~A --version"
					    *python-executable-path*)))
    :test #'string=)

  (if (uiop:version< +python-version-string+ "3.8.0")
      (defvar *python-ldflags*
        (return-value-as-list "python3-config --ldflags"))
      (defvar *python-ldflags*
        (return-value-as-list "python3-config --embed --ldflags"))))


(defvar *python-includes*
  (return-value-as-list "python3-config --includes"))

(defvar *python-site-packages-path*
  (return-value-as-list
   (format nil "~A -c 'import sys; print(\"\\n\".join(sys.path))'"
	   *python-executable-path*)))

(defun print-configuration ()
  "Prints the ldflags and includes that will be used for the compilation
of the utility shared object/library that bridges the python C-API with lisp."
  (format t "Python ldflags: ~{~A~^ ~}~%Python includes: ~{~A~^ ~}~%"
          *python-ldflags*
          *python-includes*)
  (format t "Python site path (additional):~%  ~S~%" *python-site-packages-path*)
  (format t "  These are appended to sys.path after the embedded python starts.~%")
  (format t "Python executable used for site path:~%  ~A"
          *python-executable-path*))

(defvar *disable-pystop* nil
  "If non-NIL (PY4CL2-CFFI:PYSTOP) becomes a no-op.")

(defvar *python-ignore-ldflags*
  ;; python3-config of older versions of python (such as python3.6) includes
  ;; librt in --ldflags, but apparantly, older SBCL (SBCL 1.5.4) versions fail
  ;; to load it, and librt itself seems unnecessary.
  '("-lpthread" "-ldl" "-lutil" "-lanl" "-lm" "-lrt")
  "A list of ldflags that will be ignored during the compilation of
the utility shared object/library.")

(declaim (type (member :single-threaded :multi-threaded) *python-call-mode*))
(defvar *python-call-mode* :multi-threaded
  "PY4CL2-CFFI:+PYTHON-CALL-MODE+ is a constant assigned to the value of
PY4CL2-CFFI/CONFIG:*PYTHON-CALL-MODE* at compile time. Thus, if PY4CL2-CFFI/CONFIG:*PYTHON-CALL-MODE* is changed, PY4CL2-CFFi must be recompiled.")

(defun %shared-library-from-ldflag (ldflag)
  "Given a ldflag, for example, \"-lpython3.10\", return the shared library name
corresponding to it. In this case, on linux, it will return libpython3.10.so"
  (shared-library-from-ldflag ldflag (uiop:operating-system)))

(defgeneric shared-library-from-ldflag (ldflag operating-system)
  (:documentation "This is a generic function which takes in two arguments. The first argument is an ldflag (like `-lpython3.10`) and the second argument is the `(uiop:operating-system)` as a keyword to be used for specialization on the users systems. Each method should return the shared library name associated with that ldflag and software type. For example, when `(uiop:operating-system)` is `:linux`, the relevant method should return `python3.10.so`"))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :linux)))
  (format nil "lib~A.so" (subseq ldflag 2)))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :macosx)))
  (format nil "lib~A.dylib" (subseq ldflag 2)))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :win)))
  (format nil "lib~A.dll" (subseq ldflag 2)))
