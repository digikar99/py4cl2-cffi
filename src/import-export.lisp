;; Functions and macros for importing and exporting symbols to python

;;;; Things we need to achieve - in case someone wants to attempt refactorisation
;;; For defpyfun:
;;;   - For convenience, we need to be able to show the function's arguments and
;;;   default values in Slime.
;;;   - For customizability, we ought to be able to load some "config" file
;;;   containing name, signature, documentation, call method for some functions.
;;;   This latter hasn't been attempted yet.

(in-package :py4cl2/cffi)

(defun import-function (name from &key (as nil asp))
  (declare (type string name))
  (python-start-if-not-alive)
  (foreign-funcall "PyImport_ImportModule" :string from :pointer)
  (python-may-be-error)
  (cond (asp
         (check-type as string)
         (raw-py #\x (format nil "from ~A import ~A as ~A" from name as))
         (python-may-be-error))
        (t
         (raw-py #\x (format nil "from ~A import ~A" from name))
         (python-may-be-error)))
  t)

(defun import-module (name &key (as nil asp))
  (declare (type string name))
  (python-start-if-not-alive)
  (let ((module-ptr (foreign-funcall "PyImport_ImportModule" :string name :pointer)))
    (python-may-be-error)
    (cond (asp
           (check-type as string)
           (raw-py #\x (format nil "import ~A as ~A" name as))
           (setf (py-module-pointer as) module-ptr)
           (python-may-be-error))
          (t
           (raw-py #\x (format nil "import ~A" name))
           (setf (py-module-pointer name) module-ptr)
           (python-may-be-error))))
  t)
