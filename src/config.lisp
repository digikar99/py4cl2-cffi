(defpackage :py4cl2/cffi-config
  (:use :cl)
  (:export #:*python-shared-object-path*
           #:*python-include-path*
           #:*python-additional-libraries*
           #:*python-additional-libraries-search-path*))

(in-package :py4cl2/cffi-config)

;; Use python3-config or equivalent to discover these values

;; TODO: Could set up better defaults for different OS

(declaim (type pathname
               *python-shared-object-path*
               *python-include-path*
               *python-additional-libraries-search-path*))
(defvar *python-shared-object-path*
  #P"/usr/lib/python3.8/config-3.8-x86_64-linux-gnu/libpython3.8.so")

(defun return-value-as-list (cmd)
  (mapcar (lambda (str)
            (subseq str 2))
          (remove ""
                  (uiop:split-string
                   (string-trim '(#\newline)
                                (uiop:run-program cmd
                                                  :output :string
                                                  :error-output *error-output*)))
                  :test #'string=)))

(defvar *python-include-path* (pathname (nth 0 (return-value-as-list "python3-config --includes"))))
(destructuring-bind (libpython-path more-libs-path libpython &rest more-libs)
    (return-value-as-list "python3-config --embed --ldflags")
  (defvar *python-shared-object-path*
    (pathname (uiop:strcat libpython-path "/lib" libpython ".so")))
  (defvar *python-additional-libraries* more-libs)
  (defvar *python-additional-libraries-search-path* (pathname more-libs-path)))

