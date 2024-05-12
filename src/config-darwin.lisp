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


;; One possibility is we avoid checking (software-type) or (uiop:operating-system)
;;   On non-M1, (software-type) is "Darwin".
;;   On M1, is it "64-bit Apple macOS 12.0 (Apple Silicon)”
;; However, quicklisp will try to load all the systems.
;; So, not being able to load this system on non-mac
;; will remove py4cl2-cffi from quicklisp.

;; (software-type) is not portable.
;; (uiop:operating-system) seems reasonably portable.
(when (eq :macosx (uiop:operating-system))
  (configure))
