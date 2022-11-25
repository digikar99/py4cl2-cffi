(in-package :py4cl2/cffi)

(defstruct python-object
  "A pointer to a python object which couldn't be translated into a Lisp value.
TYPE slot is the python type string
POINTER slot points to the object"
  type
  pointer)

(defvar *print-python-object* t
  "If non-NIL, python's 'str' is called on the python-object before printing.")

(defvar *python-new-references* (make-hash-table)
  "A foreign object returned as a result of python C API function that
returns a new reference should call PYTRACK.

PYGC will then decrement the references when called.")

(defun pygc ()
  (maphash-keys (lambda (key)
                  (cffi:foreign-funcall "Py_DecRef" :pointer (make-pointer key))
                  (remhash key *python-new-references*))
                *python-new-references*)
  nil)

(defun pytrack (python-object)
  "Call this function when the foreign function of the Python C-API returns
a New Reference"
  (declare (type foreign-pointer python-object)
           (optimize speed))
  (unless (null-pointer-p python-object)
    (setf (gethash (pointer-address python-object) *python-new-references*) t))
  python-object)

(defun pyuntrack (python-object)
  "Call this function when the foreign function of the Python C-API steals
a New Reference"
  (declare (type foreign-pointer python-object)
           (optimize speed))
  (let ((ht   *python-new-references*)
        (addr (pointer-address python-object)))
    (remhash addr ht))
  nil)

(defmethod print-object ((o python-object) s)
  (print-unreadable-object (o s :type t :identity t)
    (with-slots (type pointer) o
      (if *print-python-object*
          (progn
            (format s ":type ~A~%"
                    (lispify (foreign-funcall "PyObject_Str" :pointer type :pointer)))
            (pprint-logical-block (s nil :per-line-prefix "  ")
              ()
              (write-string (lispify (foreign-funcall "PyObject_Str"
                                                      :pointer pointer :pointer))
                            s))
            (terpri s))
          (format s ":POINTER ~A :TYPE ~A" pointer
                  (lispify (foreign-funcall "PyObject_Str" :pointer type :pointer)))))))

(declaim (type (function (t) foreign-pointer) pythonize))
(defgeneric pythonize (lisp-value-or-object))

(deftype c-long ()
  (let ((num-bits (* 8 (cffi:foreign-type-size :long))))
    `(signed-byte ,num-bits)))

(defmethod pythonize ((o #+sbcl sb-sys:system-area-pointer
                         #-sbcl foreign-pointer))
  o)
(defmethod pythonize ((o python-object)) (python-object-pointer o))

(defmethod pythonize ((o integer))
  (unless (typep o 'c-long)
    ;; TODO: Proper warning class
    (warn "Given integer ~S is too bit to be interpreted as a C long" o))
  (pytrack (foreign-funcall "PyLong_FromLong" :long o :pointer)))

(defmethod pythonize ((o float))
  ;; TODO: Different numpy float types: float32 and float64
  (pytrack (foreign-funcall "PyFloat_FromDouble"
                            :double (coerce o 'double-float)
                            :pointer)))

(defmethod pythonize ((o string))
  (pytrack (foreign-funcall "PyUnicode_FromString" :string o :pointer)))

(defmethod pythonize ((o list))
  (pytrack
   (let ((tuple (foreign-funcall "PyTuple_New" :int (length o) :pointer)))
     (loop :for elt :in o
           :for pyelt := (pythonize elt)
           :for pos :from 0
           :do (if (zerop (foreign-funcall "PyTuple_SetItem"
                                           :pointer tuple
                                           :int pos
                                           :pointer pyelt
                                           :int))
                   (pyuntrack pyelt)
                   (python-may-be-error)))
     tuple)))

(defmethod pythonize ((o vector))
  (if (typep o '(vector t))
      (pytrack
       (let ((list (foreign-funcall "PyList_New" :int (length o) :pointer)))
         (loop :for elt :across o
               :for pyelt := (pythonize elt)
               :for pos :from 0
               :do (if (zerop (foreign-funcall "PyList_SetItem"
                                               :pointer list
                                               :int pos
                                               :pointer pyelt
                                               :int))
                       (pyuntrack pyelt)
                       (python-may-be-error)))
         list))
      (pythonize-array o)))

(defmethod pythonize ((o array))
  (pythonize-array o))

(defun pythonize-array (array)
  (pytrack
   (let* ((typenum (foreign-funcall "PyArray_typenum_from_element_type"
                                    :string (array-element-typecode array)
                                    :int))
          (py-array-descr (numpy-funcall "PyArray_DescrFromType" :int typenum :pointer))
          (ndarray-type   (foreign-funcall "PyDict_GetItemString"
                                           :pointer (py-module-dict "numpy")
                                           :string "ndarray"
                                           :pointer))
          (ndims          (array-rank array)))
     (with-foreign-objects ((dims    :long ndims))
       (dotimes (i ndims)
         (setf (mem-aref dims :long i) (array-dimension array i)))
       (with-pointer-to-vector-data (array-data (sb-ext:array-storage-vector array))
         (numpy-funcall "PyArray_NewFromDescr"
                        :pointer ndarray-type
                        :pointer py-array-descr
                        :int ndims
                        :pointer dims
                        :pointer (null-pointer)
                        :pointer array-data
                        :int (logior +npy-array-c-contiguous+ +npy-array-writeable+) ; flags
                        :pointer (null-pointer)
                        :pointer))))))

(defun array-element-typecode (array)
  (declare (optimize speed))
  (eswitch ((array-element-type array) :test #'type=)
    ('single-float "f32")
    ('double-float "f64")
    ('(signed-byte 64) "sb64")
    ('(signed-byte 32) "sb32")
    ('(signed-byte 16) "sb16")
    ('(signed-byte 08) "sb8")
    ('(unsigned-byte 64) "ub64")
    ('(unsigned-byte 32) "ub32")
    ('(unsigned-byte 16) "ub16")
    ('(unsigned-byte 08) "ub8")))

(defun pythonize-list (list)
  (pytrack
   (let ((tuple (foreign-funcall "PyTuple_New" :int (length list) :pointer)))
     (loop :for elt :in list
           :for pyelt := (pythonize elt)
           :for pos :from 0
           :do (assert (zerop (foreign-funcall "PyTuple_SetItem"
                                               :pointer tuple
                                               :int pos
                                               :pointer pyelt
                                               :int)))
               (pyuntrack pyelt))
     tuple)))

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
  (pytrack
   (if (null plist)
       (null-pointer)
       (let ((dict (foreign-funcall "PyDict_New" :pointer)))
         (doplist (key val plist)
                  (assert (zerop (foreign-funcall "PyDict_SetItem"
                                                  :pointer dict
                                                  :pointer (pythonize key)
                                                  :pointer (pythonize val)
                                                  :int))))
         dict))))
