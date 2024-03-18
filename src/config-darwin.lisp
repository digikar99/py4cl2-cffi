;;;; config.lisp

(defpackage :py4cl2-cffi/config-darwin
  (:documentation "Configures macOS operating systems.")
  (:use #:cl
        #:cl-ppcre
        :py4cl2-cffi/config))
(in-package #:py4cl2-cffi/config-darwin)

(defun python-executable ()
  "Determine if the name of the python executable."
  (or (multiple-value-bind (_ e)
          (ignore-errors (uiop:run-program "which python"))
        (declare (ignore _))
        (unless e "python"))
      (multiple-value-bind (_ e)
          (ignore-errors (uiop:run-program "which python3"))
        (declare (ignore _))
        (unless e "python3"))
      (error "Python executable not found.")))

(defun python-system ()
  "The path to the Python install or where the virtual environment originates."
  (read-from-string
   (with-output-to-string (stream)
     (uiop:run-program
      (format nil
              "~a -c \"
import sys
print(f'(:base-exec-prefix \\\"{sys.base_exec_prefix}\\\"' +
      f' :exec-prefix \\\"{sys.exec_prefix}\\\")')\""
              (python-executable))
      :output stream)
     stream)))

(defun configure ()
  (let* ((ps (python-system))
         (prefix (getf ps :base-exec-prefix))
         (path (format nil "~A/" prefix))
         (search-path (getf ps :exec-prefix))
         (python-version (ppcre:register-groups-bind (version)
                             ("^.+\/(.+)?$" prefix :sharedp t)
                           version)))
    (setq py4cl2-cffi/config:*python-ldflags*
          (list (format nil "-L'~A' -L'lib/~A' -l'~A'"
                        (make-pathname :name search-path)
                        path
                        (format nil "python~A" python-version)))

          py4cl2-cffi/config:*python-compile-command*
          (concatenate
           'string
           "gcc ~A -I'~A' -c -Wall -Werror -fpic py4cl-utils.c && "
           (format
            nil
            "gcc -L'~A/lib' -shared -o libpy4cl-utils.so py4cl-utils.o -lpython~A"
            path python-version)))))

(if (equal "Darwin" (software-type))
    (configure))
