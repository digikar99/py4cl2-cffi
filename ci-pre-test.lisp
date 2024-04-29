(in-package :cl-user)
;; (ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2020-10-16/distinfo.txt"
;;                       :replace t
;;                       :prompt nil)
(push :ci *features*)
(push #P"~/" ql:*local-project-directories*)
(format t "~%Loading TRIVIAL-BACKTRACE...")
(ql:quickload "trivial-backtrace")
(format t "DONE~%")
(print (ql:where-is-system "py4cl2-cffi"))
(ql:quickload "trivial-backtrace")
(format t "~%TRIVIAL-BACKTRACE loaded.~%")
(force-output)
(defmacro print-and-call (form)
  `(progn
     (format t "~%Executing ~S~%" ',form)
     ,form
     (format t "~%DONE Executing ~S~%" ',form)))
(ql:quickload "py4cl2-cffi-tests")
(uiop:quit 0)
