(in-package :py4cl2-cffi)

(defvar *py-type-lispifier-table* ())

(defmacro define-lispifier (name (pyobject-var) &body body)
  (declare (type string name))
  `(setf (assoc-value *py-type-lispifier-table* ,name :test #'string=)
         (lambda (,pyobject-var) ,@body)))

(define-lispifier "int" (o)
  (pyforeign-funcall "PyLong_AsLong" :pointer o :long))

(define-lispifier "float" (o)
  (pyforeign-funcall "PyFloat_AsDouble" :pointer o :double))

(define-lispifier "str" (o)
  (nth-value 0
             (foreign-string-to-lisp
              (pyforeign-funcall "PyUnicode_AsUTF8" :pointer o :pointer))))

(define-lispifier "bool" (o)
  (cond ((pointer-eq o (pyvalue* "True"))
         t)
        ((pointer-eq o (pyvalue* "False"))
         nil)
        (t
         (error "Object at ~S is a bool but is neither 'True' or 'False'" o))))

(define-lispifier "tuple" (o)
  (let ((py-size (pyforeign-funcall "PyTuple_Size" :pointer o :int)))
    (loop :for i :below py-size
          :collect (lispify (pyforeign-funcall "PyTuple_GetItem" :pointer o
                                                                 :int i
                                                                 :pointer)))))

(define-lispifier "list" (o)
  (let* ((py-size (pyforeign-funcall "PyList_Size" :pointer o :int))
         (vec     (make-array py-size :element-type t)))
    (loop :for i :below py-size
          :do (setf (svref vec i)
                    (lispify (pyforeign-funcall "PyList_GetItem" :pointer o
                                                                 :int i
                                                                 :pointer))))
    vec))

(define-lispifier "dict" (o)
  (let ((py-size (pyforeign-funcall "PyDict_Size" :pointer o :long))
        (hash-table (make-hash-table :test #'equalp))
        (py-keys (pyforeign-funcall "PyDict_Keys" :pointer o :pointer)))
    (loop :for i :below py-size
          :for py-key := (pyforeign-funcall "PyList_GetItem"
                                            :pointer py-keys
                                            :int i
                                            :pointer)
          :for key := (lispify py-key)
          :for value := (lispify (pyforeign-funcall "PyDict_GetItem"
                                                    :pointer o
                                                    :pointer py-key
                                                    :pointer))
          :do (setf (gethash key hash-table) value))
    hash-table))

(define-lispifier "numpy.ndarray" (o)
  (let* ((dims     (pyslot-value o "shape"))
         (element-type (let* ((*read-eval* nil)
                              (*package* (find-package :cl)))
                         (read-from-string
                          (foreign-string-to-lisp
                           (pyforeign-funcall "PyArray_element_type_from_array"
                                              :pointer o :pointer)))))
         (from-vec  (pyforeign-funcall "PyArray_Data" :pointer o :pointer))
         ;; FIXME: Strides
         (array     (make-array dims :element-type element-type))
         (num-bytes (* (array-element-type-num-bytes array)
                       (reduce #'* dims :initial-value 1))))
    (with-pointer-to-vector-data (to-vec (array-storage array))
      (pyforeign-funcall "memcpy" :pointer to-vec :pointer from-vec :int num-bytes))
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
  (let* ((pyobject-type (pyforeign-funcall "PyObject_Type"
                                           :pointer pyobject
                                           :pointer))
         (pytype-name (if (null-pointer-p pyobject-type)
                          nil
                          (foreign-string-to-lisp
                           (pyforeign-funcall "PyTypeObject_Name"
                                              :pointer pyobject-type
                                              :pointer))))
         ;; FIXME: What about names in modules?
         (lispifier (assoc-value *py-type-lispifier-table* pytype-name :test #'string=)))
    (pyobject-tracked-p pyobject)
    (customize
     (cond ((null-pointer-p pyobject-type)
            nil)
           ((string= pytype-name "NoneType")
            nil)
           ((null lispifier)
            (pyuntrack pyobject)
            (pyuntrack pyobject-type)
            ;; (if (pyobject-tracked-p pyobject)
            ;;     (progn
            ;;       (make-tracked-python-object :pointer pyobject :type pyobject-type))
            ;;     (make-python-object :pointer pyobject :type pyobject-type))
            (make-tracked-python-object :pointer pyobject :type pyobject-type)
            )
           (t
            (funcall lispifier pyobject))))))

(defvar *lispifiers*
  ()
  ;; FIXME: Making new objects can be expensive
  "Each entry in the alist *LISPIFIERS* maps from a lisp-type to
a single-argument lisp function. This function takes as input the \"default\" lisp
objects and is expected to appropriately parse it to the corresponding lisp object.

NOTE: This is a new feature and hence unstable; recommended to avoid in production code.")

(defmacro with-lispifiers ((&rest overriding-lispifiers) &body body)
  "Each entry of OVERRIDING-LISPIFIERS is a two-element list of the form
  (TYPE LISPIFIER)
Here, TYPE is unevaluated, while LISPIFIER will be evaluated; the LISPIFIER is expected
to take a default-lispified object (see lisp-python types translation table in docs)
and return the appropriate object user expects.

For example,

  (raw-pyeval \"[1, 2, 3]\") ;=> #(1 2 3) ; the default lispified object
  (with-lispifiers ((vector (lambda (x) (coerce (print x) 'list))))
    (print (raw-pyeval \"[1,2,3]\"))
    (print (raw-pyeval 5)))
  ; #(1 2 3) ; default lispified object
  ; (1 2 3)  ; coerced to LIST by the lispifier
  ; 5        ; lispifier uncalled for non-VECTOR
  5

NOTE: This is a new feature and hence unstable; recommended to avoid in production code."
  `(let ((*lispifiers* (list* ,@(loop :for (type lispifier) :in overriding-lispifiers
                                      :collect `(cons ',type ,lispifier))
                              *lispifiers*)))
     ,@body))

(defun customize (object)
  (loop :for (type . lispifier) :in *lispifiers*
        :if (typep object type)
          :do (return-from customize (funcall lispifier object)))
  object)
