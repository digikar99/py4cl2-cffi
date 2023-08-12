;;;; config.lisp

(defpackage :py4cl2-cffi/config-darwin
  (:documentation "Configures macOS operating systems.")
  (:use #:cl
        #:cl-ppcre
        :py4cl2-cffi/config))
(in-package #:py4cl2-cffi/config-darwin)

(defun python-system ()
  "The path to the Python install or where the virtual environment originates."
  (read-from-string
   (with-output-to-string (stream)
     (uiop:run-program "python -c \"
import sys
print(f'(:base-exec-prefix \\\"{sys.base_exec_prefix}\\\"' +
      f' :exec-prefix \\\"{sys.exec_prefix}\\\")')\""
                       :output stream)
     stream)))

(defun configure ()
  (let* ((ps (python-system))
         (prefix (getf ps :base-exec-prefix))
         (search-path (getf ps :exec-prefix))
         (python-version (ppcre:register-groups-bind (version)
                             ("^.+\/(.+)?$" prefix :sharedp t)
                           version))
         (path (format nil "~A/" prefix)))
    (setq
     py4cl2-cffi/config:*python-shared-object-path*
     (merge-pathnames (format nil "lib/libpython~A.dylib" python-version) path)

     py4cl2-cffi/config:*python-additional-libraries-search-path*
     (make-pathname :name search-path)

     py4cl2-cffi/config:*python-compile-command*
     (concatenate
      'string
      "gcc -I'~A' -I'~A' -c -Wall -Werror -fpic py4cl-utils.c && "
      (format
       nil
       "gcc -L'~A/lib' -shared -o libpy4cl-utils.so py4cl-utils.o -lpython~A"
       path python-version)))))

(if (equal "Darwin" (software-type))
    (configure))
