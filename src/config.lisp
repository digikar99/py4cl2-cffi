(defpackage :py4cl2-cffi/config
  (:use :cl)
  (:export #:*python-ldflags*
           #:*python-ignore-ldflags*
           #:*python-includes*
           #:*python-compile-command*
           #:*python-numpy-compile-command*
           #:*python-executable-path*
           #:*python-site-packages-path*
           #:print-configuration
           #:shared-library-from-ldflag))

(in-package :py4cl2-cffi/config)

;; Use python3-config or equivalent to discover these values

;; TODO: Could set up better defaults for different OS

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
                    (print
                     (uiop:split-string
                      (uiop:run-program cmd
                                        :output :string
                                        :error-output *error-output*)
                      :separator '(#\newline #\tab #\space))))
            :test #'string=))

  (let ((python-version-string
          (second (return-value-as-list "python3 --version"))))
    (if (uiop:version< python-version-string "3.8.0")
        (defvar *python-ldflags*
          (return-value-as-list "python3-config --ldflags"))
        (defvar *python-ldflags*
          (return-value-as-list "python3-config --embed --ldflags")))))


(defvar *python-includes*
  (return-value-as-list "python3-config --includes"))

(defun print-configuration ()
  "Prints the ldflags and includes that will be used for the compilation
of the utility shared object/library that bridges the python C-API with lisp."
  (format t "Python ldflags: ~{~A~^ ~}~%Python includes: ~{~A~^ ~}~%"
          *python-ldflags*
          *python-includes*))

(defvar *python-executable-path*
  (first (return-value-as-list "which python"))
  "The path to python executable. This will be used to set sys.path
in cases (such as venv) when python3-config is not available.

FIXME: We should actually use pyvenv.cfg to set sys.path.")

(defvar *python-site-packages-path*
  (return-value-as-list
   (format nil "find '~A' -name 'site-packages'"
           (namestring
            (uiop:pathname-parent-directory-pathname
             *python-executable-path*)))))

(defvar *python-ignore-ldflags*
  ;; python3-config of older versions of python (such as python3.6) includes
  ;; librt in --ldflags, but apparantly, older SBCL (SBCL 1.5.4) versions fail
  ;; to load it, and librt itself seems unnecessary.
  '("-lpthread" "-ldl" "-lutil" "-lanl" "-lm" "-lrt")
  "A list of ldflags that will be ignored during the compilation of
the utility shared object/library.")

(defun %shared-library-from-ldflag (ldflag)
  "Given a ldflag, for example, \"-lpython3.10\", return the shared library name
corresponding to it. In this case, on linux, it will return libpython3.10.so"
  (shared-library-from-ldflag ldflag
                              (intern (string-upcase (software-type))
                                      :keyword)))

(defgeneric shared-library-from-ldflag (ldflag software-type)
  (:documentation "This is a generic function which takes in two arguments. The first argument is an ldflag (like `-lpython3.10`) and the second argument is the `(software-type)` as a keyword to be used for specialization on the users systems. Each method should return the shared library name associated with that ldflag and software type. For example, when `(intern (string-upcase (software-type)) :keyword)` is `:linux`, the relevant method should return `python3.10.so`"))

(defmethod shared-library-from-ldflag (ldflag (software-type (eql :linux)))
  (format nil "lib~A.so" (subseq ldflag 2)))

(defmethod shared-library-from-ldflag (ldflag (software-type (eql :darwin)))
  (format nil "lib~A.dylib" (subseq ldflag 2)))

(defmethod shared-library-from-ldflag (ldflag (software-type (eql :windows)))
  (format nil "lib~A.dll" (subseq ldflag 2)))
