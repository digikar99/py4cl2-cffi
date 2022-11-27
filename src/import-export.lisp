(in-package :py4cl2/cffi)

;; (defmacro defpymodule)

(defun import-module (name &key (as nil asp))
  (declare (type string name))
  (python-start-if-not-alive)
  (let ((module-ptr (foreign-funcall "PyImport_ImportModule" :string name :pointer)))
    (python-may-be-error)
    (when asp
      (check-type as string)
      (raw-py (format nil "import ~A as ~A" name as))
      (setf (py-module-pointer as) module-ptr)
      (python-may-be-error)))
  t)
