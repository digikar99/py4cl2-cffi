(in-package :cl-user)
;; (ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2020-10-16/distinfo.txt"
;;                       :replace t
;;                       :prompt nil)
(push :ci *features*)
(push #P"~/" ql:*local-project-directories*)
(print (ql:where-is-system "py4cl2-cffi"))
(force-output)
(defmacro print-and-call (form)
  `(progn
     (format t "~%Executing ~S~%" ',form)
     ,form
     (format t "~%DONE Executing ~S~%" ',form)))
(print-and-call (ql:quickload "py4cl2-cffi"))
(print-and-call (py4cl2-cffi:pystart))
(print-and-call (py4cl2-cffi:pystart))
(print-and-call (py4cl2-cffi:import-module "math"))
(print-and-call
 (py4cl2-cffi:defpymodule "math" nil :continue-ignoring-errors nil))
;; (ql:quickload "py4cl2-cffi-tests")
(uiop:quit 0)
