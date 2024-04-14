(in-package :py4cl2-cffi/single-threaded)

(defun raw-py (cmd-char &rest code-strings)
  "CMD-CHAR should be #\e for eval and #\x for exec.

Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (python-start-if-not-alive)
  (with-python-gil
    (let* ((return-code nil)
           (command (ecase cmd-char
                      (#\e (apply #'concatenate 'string "_ = " code-strings))
                      (#\x (apply #'concatenate 'string code-strings))))
           (error-output
             (ecase *python-state*
               (:initialized
                (setq return-code
                      (pyforeign-funcall "PyRun_SimpleString" :string command :int)))
               (:initializing
                (setq return-code
                      (pyforeign-funcall "PyRun_SimpleString" :string command :int))
                ""))))
      (unless (zerop return-code)
        (error 'pyerror
               :format-control error-output))
      (ecase cmd-char
        (#\e (let ((ptr (pyvalue* "_")))
               (pyforeign-funcall "Py_IncRef" :pointer ptr)
               (pytrack ptr)))
        (#\x (values))))))

