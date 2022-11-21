(in-package :py4cl2/cffi)

(defvar *py-type-lispifier-table* ())

(defmacro define-lispifier (name (pyobject-var) &body body)
  (declare (type string name))
  `(setf (assoc-value *py-type-lispifier-table* (pytype ,name) :test #'pointer-eq)
         (lambda (,pyobject-var) ,@body)))

;; FIXME: Should the checks be exact?
(define-lispifier "int" (o)
  (assert (not (zerop (foreign-funcall "PyCheck_Long" :pointer o :int))))
  (foreign-funcall "PyLong_AsLong" :pointer o :long))

(define-lispifier "float" (o)
  (assert (not (zerop (foreign-funcall "PyCheck_Float" :pointer o :int))))
  (foreign-funcall "PyLong_AsLong" :pointer o :long))

(define-lispifier "str" (o)
  (assert (not (zerop (foreign-funcall "PyCheck_Unicode" :pointer o :int))))
  (nth-value 0
             (foreign-string-to-lisp (foreign-funcall "PyUnicode_AsUTF8" :pointer o :pointer))))

(define-lispifier "tuple" (o)
  (assert (not (zerop (foreign-funcall "PyCheck_Tuple" :pointer o :int))))
  (let ((py-size (foreign-funcall "PyTuple_Size" :pointer o :int)))
    (loop :for i :below py-size
          :collect (lispify (foreign-funcall "PyTuple_GetItem" :pointer o
                                                                      :int i
                                                                      :pointer)))))

(define-lispifier "list" (o)
  (assert (not (zerop (foreign-funcall "PyCheck_List" :pointer o :int))))
  (let* ((py-size (foreign-funcall "PyList_Size" :pointer o :int))
         (vec     (make-array py-size :element-type t)))
    (loop :for i :below py-size
          :do (setf (svref vec i)
                    (lispify (foreign-funcall "PyList_GetItem" :pointer o
                                                                      :int i
                                                                      :pointer))))
    vec))

(define-lispifier "dict" (o)
  (assert (not (zerop (foreign-funcall "PyCheck_Dict" :pointer o :int))))
  (let ((py-size (foreign-funcall "PyDict_Size" :pointer o :int))
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

(defstruct python-object
  "A pointer to a python object which couldn't be translated into a Lisp value.
TYPE slot is the python type string
POINTER slot points to the object"
  type
  pointer)

(defvar *print-python-object* t
  "If non-NIL, python's 'str' is called on the python-object before printing.")

(defmethod print-object ((o python-object) s)
  (print-unreadable-object (o s :type t :identity t)
    (with-slots (type pointer) o
      (if *print-python-object*
          (progn
            (terpri s)
            (pprint-logical-block (s nil :per-line-prefix "  ")
              (write-string (lispify (foreign-funcall "PyObject_Str" :pointer pointer :pointer))
                            s))
            (terpri s))
          (format s ":POINTER ~A :TYPE ~A" pointer
                  (lispify (foreign-funcall "PyObject_Str" :pointer type :pointer)))))))

(defun lispify (pyobject)
  (declare (type foreign-pointer pyobject))
  (let* ((pyobject-type (foreign-funcall "PyObject_Type"
                                         :pointer pyobject
                                         :pointer))
         (lispifier (assoc-value *py-type-lispifier-table* pyobject-type :test #'pointer-eq)))
    ;; (print (list pyobject-type lispifier))
    (if lispifier
        (funcall lispifier pyobject)
        (make-python-object :pointer pyobject :type pyobject-type))))
