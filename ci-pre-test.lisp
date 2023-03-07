(in-package :cl-user)
;; (ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2020-10-16/distinfo.txt"
;;                       :replace t
;;                       :prompt nil)
(push :travis *features*)
(push #P"~/" ql:*local-project-directories*)
(print (ql:where-is-system "py4cl2-cffi"))
(ql:quickload "py4cl2-cffi-tests")
(uiop:quit 0)
