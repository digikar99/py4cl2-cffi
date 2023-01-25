(in-package :py4cl2-cffi)

(defmacro with-python-gil (&body body)
  (with-gensyms (gil)
    `(let ((,gil (foreign-funcall "PyGILState_Ensure" :pointer)))
       (declare (optimize debug))
       (unwind-protect (locally ,@body)
         (foreign-funcall "PyGILState_Release" :pointer ,gil)))))

(defmacro pyforeign-funcall (name-and-options &rest args)
  "Wraps CFFI:FOREIGN-FUNCALL in WITH-PYTHON-GIL"
  `(with-python-gil
     (foreign-funcall ,name-and-options ,@args)))
