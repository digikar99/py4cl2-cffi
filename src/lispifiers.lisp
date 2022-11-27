(in-package :py4cl2/cffi)

(defvar *py-type-lispifier-table* ())

(defmacro define-lispifier (name (pyobject-var) &body body)
  (declare (type string name))
  `(setf (assoc-value *py-type-lispifier-table* ,name :test #'string=)
         (lambda (,pyobject-var) ,@body)))

(define-lispifier "int" (o)
  (foreign-funcall "PyLong_AsLong" :pointer o :long))

(define-lispifier "float" (o)
  (foreign-funcall "PyFloat_AsDouble" :pointer o :double))

(define-lispifier "str" (o)
  (nth-value 0
             (foreign-string-to-lisp (foreign-funcall "PyUnicode_AsUTF8" :pointer o :pointer))))

(define-lispifier "tuple" (o)
  (let ((py-size (foreign-funcall "PyTuple_Size" :pointer o :int)))
    (loop :for i :below py-size
          :collect (lispify (foreign-funcall "PyTuple_GetItem" :pointer o
                                                               :int i
                                                               :pointer)))))

(define-lispifier "list" (o)
  (let* ((py-size (foreign-funcall "PyList_Size" :pointer o :int))
         (vec     (make-array py-size :element-type t)))
    (loop :for i :below py-size
          :do (setf (svref vec i)
                    (lispify (foreign-funcall "PyList_GetItem" :pointer o
                                                               :int i
                                                               :pointer))))
    vec))

(define-lispifier "dict" (o)
  (let ((py-size (foreign-funcall "PyDict_Size" :pointer o :long))
        (hash-table (make-hash-table :test #'equalp))
        (py-keys (foreign-funcall "PyDict_Keys" :pointer o :pointer)))
    (loop :for i :below py-size
          :for py-key := (foreign-funcall "PyList_GetItem"
                                          :pointer py-keys
                                          :int i
                                          :pointer)
          :for key := (lispify py-key)
          :for value := (lispify (foreign-funcall "PyDict_GetItem"
                                                  :pointer o
                                                  :pointer py-key
                                                  :pointer))
          :do (setf (gethash key hash-table) value))
    hash-table))

(define-lispifier "numpy.ndarray" (o)
  (declare (optimize debug))
  (let* ((dims     (pyslot-value o "shape"))
         (element-type (let* ((*read-eval* nil)
                              (*package* (find-package :cl)))
                         (read-from-string
                          (foreign-string-to-lisp
                           (foreign-funcall "PyArray_element_type_from_array"
                                            :pointer o :pointer)))))
         (from-vec  (foreign-funcall "PyArray_Data" :pointer o :pointer))
         (array     (make-array dims :element-type element-type))
         (num-bytes (* (array-element-type-num-bytes array)
                       (reduce #'* dims :initial-value 1))))
    (with-pointer-to-vector-data (to-vec (sb-ext:array-storage-vector array))
      (foreign-funcall "memcpy" :pointer to-vec :pointer from-vec :int num-bytes))
    array))

(defun array-element-type-num-bytes (array)
  (eswitch ((array-element-type array) :test #'type=)
    ('single-float 4)
    ('double-float 8)
    ('(signed-byte 64) 8)
    ('(signed-byte 32) 4)
    ('(signed-byte 16) 2)
    ('(signed-byte 08) 1)
    ('(unsigned-byte 64) 8)
    ('(unsigned-byte 32) 4)
    ('(unsigned-byte 16) 2)
    ('(unsigned-byte 08) 1)))

(defun lispify (pyobject)
  (declare (type foreign-pointer pyobject))
  (let* ((pyobject-type (foreign-funcall "PyObject_Type"
                                         :pointer pyobject
                                         :pointer))
         (pytype-name (if (null-pointer-p pyobject-type)
                          nil
                          (foreign-string-to-lisp
                           (foreign-funcall "PyTypeObject_Name"
                                            :pointer (pytrack pyobject-type)
                                            :pointer))))
         ;; FIXME: What about names in modules?
         (lispifier (assoc-value *py-type-lispifier-table* pytype-name :test #'string=)))
    (cond ((null-pointer-p pyobject-type)
           nil)
          ((null lispifier)
           (make-python-object :pointer pyobject :type pyobject-type))
          (t
           (funcall lispifier pyobject)))))
