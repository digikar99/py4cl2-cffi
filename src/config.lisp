(defpackage :py4cl2-cffi/config
  (:use :cl)
  (:export #:*python-ldflags*
           #:*python-includes*
           #:*python-compile-command*
           #:print-configuration))

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
   ;; The first ~A corresponds to the *python-includes* defined below.
   ;; The second ~A corresponds to the numpy include files discovered
   ;;   in shared-objects.lisp
   "gcc ~A -I'~A' -c -Wall -Werror -fpic py4cl-utils.c && "
   "gcc -shared -o libpy4cl-utils.so py4cl-utils.o"))

(defun return-value-as-list (cmd)
  (remove ""
          (uiop:split-string
           (string-trim '(#\newline)
                        (uiop:run-program cmd
                                          :output :string
                                          :error-output *error-output*)))
          :test #'string=))

(let ((python-version-string (second (return-value-as-list "python3 --version"))))
  (if (uiop:version< python-version-string "3.8.0")
      (defvar *python-ldflags* (return-value-as-list "python3-config --ldflags"))
      (defvar *python-ldflags*
        (return-value-as-list "python3-config --embed --ldflags"))))

(defvar *python-includes*
  (return-value-as-list "python3-config --includes"))

(defun print-configuration ()
  (format t "Python ldflags: ~A~%Python includes: ~A~%"
          *python-ldflags*
          *python-includes*))
