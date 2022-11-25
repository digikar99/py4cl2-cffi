(in-package :py4cl2/cffi)

;; (defmacro defpymodule)

(defun import-module (name)
  (declare (type string name))
  (python-start-if-not-alive)
  (foreign-funcall "PyImport_ImportModule" :string name :pointer))
