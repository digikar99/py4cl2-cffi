(in-package :py4cl2-cffi)

(declaim (inline pyerror-stop-iteration-p))
(defun pyerror-stop-iteration-p (simple-pyerror)
  (declare (optimize speed)
           (type simple-pyerror simple-pyerror))
  (string= "StopIteration" (slot-value simple-pyerror 'type)))

(deftype pyerror-stop-iteration ()
  `(and simple-pyerror
        (satisfies pyerror-stop-iteration-p)))

(defmacro-clause (PYFOR var IN pyobject-wrapper)
  ;; TODO: Tests and docstrings
  (let ((pygen (gensym "PYGENERATOR")))
    `(progn
       (with ,pygen = (pycall "iter" ,pyobject-wrapper))
       (for ,var = (handler-case (pycall "next" ,pygen)
                     (pyerror-stop-iteration () (finish)))))))
