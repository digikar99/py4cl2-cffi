(in-package :py4cl2/cffi)

(declaim (type (function (t) foreign-pointer)))
(defgeneric pythonize (lisp-value-or-object))

(deftype c-long ()
  (let ((num-bits (* 8 (cffi:foreign-type-size :long))))
    `(signed-byte ,num-bits)))

(defmethod pythonize ((o integer))
  (unless (typep o 'c-long)
    ;; TODO: Proper warning class
    (warn "Given integer ~S is too bit to be interpreted as a C long" o))
  (foreign-funcall "PyLong_FromLong" :long o :pointer))

(defmethod pythonize ((o float))
  ;; TODO: Different numpy float types: float32 and float64
  (foreign-funcall "PyFloat_FromDouble" :double (coerce o 'double-float) :pointer))

(defmethod pythonize ((o string))
  (foreign-funcall "PyUnicode_FromString" :string o :pointer))

(defmethod pythonize ((o list))
  (let ((tuple (foreign-funcall "PyTuple_New" :int (length o) :pointer)))
    (loop :for elt :in o
          :for pos :from 0
          :do (unless (zerop (foreign-funcall "PyTuple_SetItem"
                                              :pointer tuple
                                              :int pos
                                              :pointer (pythonize elt)
                                              :int))
                (python-may-be-error)))
    tuple))

(defmethod pythonize ((o vector))
  (let ((list (foreign-funcall "PyList_New" :int (length o) :pointer)))
    (loop :for elt :across o
          :for pos :from 0
          :do (unless (zerop (foreign-funcall "PyList_SetItem"
                                              :pointer list
                                              :int pos
                                              :pointer (pythonize elt)
                                              :int))
                (python-may-be-error)))
    list))

(defun pythonize-list (list)
  (let ((tuple (foreign-funcall "PyTuple_New" :int (length list) :pointer)))
    (loop :for elt :in list
          :for pos :from 0
          :do (assert (zerop (foreign-funcall "PyTuple_SetItem"
                                              :pointer tuple
                                              :int pos
                                              :pointer (pythonize elt)
                                              :int))))
    tuple))

(defmethod pythonize ((o symbol))
  (if (null o)
      (null-pointer)
      (let* ((name (symbol-name o))
             (len  (length name)))
        (pythonize (cond ((and (eq #\* (char name 0))
                               (eq #\* (char name (1- len))))
                          (nstring-upcase (subseq name 1 (- len 2))))
                         ((every #'upper-case-p name)
                          (string-downcase name))
                         (t
                          name))))))

(defun pythonize-plist (plist)
  (if (null plist)
      (null-pointer)
      (let ((dict (foreign-funcall "PyDict_New" :pointer)))
        (doplist (key val plist)
           (assert (zerop (foreign-funcall "PyDict_SetItem"
                                           :pointer dict
                                           :pointer (pythonize key)
                                           :pointer (pythonize val)
                                           :int))))
        dict)))
