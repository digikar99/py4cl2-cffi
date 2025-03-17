(in-package :cl-user)

(push :ci *features*)
(when (eq :linux (uiop:operating-system))
  (push (print (pathname (uiop:getenv "EXOTIC_DIR")))
        ql:*local-project-directories*))
(push #P"./" ql:*local-project-directories*)

(ql:quickload "py4cl2-cffi-tests")
    
(py4cl2-cffi/config:print-configuration)
    
(let ((report (py4cl2-cffi-tests:run)))
  (when (or (plusp (slot-value report 'clunit::failed))
	    (plusp (slot-value report 'clunit::errors)))
    (uiop:quit 1)))
(terpri)
(format t "!!MULTI-THREADED TESTS RAN SUCCESSFULLY!!")

#-:ccl
(progn
  (ql:quickload "py4cl2-cffi/single-threaded")
  ;; (py4cl2-cffi/single-threaded:pystart)
  (funcall (symbol-function (intern "PYSTART" (find-package "PY4CL2-CFFI/SINGLE-THREADED"))))
  ;; (assert (string= "42" (py4cl2-cffi/single-threaded:pycall "str" 42)))
  (assert (string= "42" (funcall (symbol-function (intern "PYCALL" (find-package "PY4CL2-CFFI/SINGLE-THREADED"))) "str" 42)))
  (terpri)
  (format t "!!Preliminary SINGLE-THREADED TESTS RAN SUCCESSFULLY!!")
  (uiop:quit 0))
