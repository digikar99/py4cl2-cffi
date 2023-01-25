(in-package :py4cl2-cffi)

(defun pygil-held-p ()
  ;; Python's CAPI documentation lists this as mainly a helper/diagnostic function.
  ;; We haven't used this anywhere.
  (if (= 0 (foreign-funcall "PyGILState_Check" :int))
      nil
      t))

(defun pygil-release (&optional (ptr (null-pointer))) ; default value seems 0 on linux
  (if (pygil-held-p)
      (progn
        (foreign-funcall "PyGILState_Release" :pointer ptr)
        t)
      nil))

(defun pygil-ensure ()
  (foreign-funcall "PyGILState_Ensure" :pointer))

(defmacro with-python-gil (&body body)
  (with-gensyms (gil)
    `(let ((,gil (foreign-funcall "PyGILState_Ensure" :pointer)))
       (unwind-protect (locally ,@body)
         (foreign-funcall "PyGILState_Release" :pointer ,gil)))))

(define-constant +python-function-reference-type-alist+
    '(("Py_Initialize"      nil)
      ("Py_IsInitialized"   nil)
      ("Py_FinalizeEx"      nil)
      ("PyRun_SimpleString" nil)
      ("Py_IncRef"          nil)
      ("Py_DecRef"          nil)
      ("PyObject_Call"      :new)
      ("PyObject_Str"       :new)
      ("PyObject_Type"      :new)
      ("PyObject_GetAttrString" :new)
      ("PyObject_SetAttrString" nil)
      ("PyLong_FromLong"    :new)
      ("PyLong_AsLong"      nil)
      ("PyFloat_FromDouble" :new)
      ("PyFloat_AsDouble"   nil)
      ("PyUnicode_FromString" :new)
      ("PyUnicode_AsUTF8"   nil)
      ("PyList_New"         :new)
      ("PyList_Size"        nil)
      ("PyList_GetItem"     :borrowed)
      ("PyList_SetItem"     nil) ; arguments
      ("PyDict_New"         :new)
      ("PyDict_SetItem"     nil)
      ("PyDict_GetItem"     :borrowed)
      ("PyDict_GetItemString" :borrowed)
      ("PyDict_SetItemString" nil)
      ("PyDict_Size"        nil)
      ("PyDict_Keys"        :new)
      ("PyTuple_New"        :new)
      ("PyTuple_Size"       nil)
      ("PyTuple_SetItem"    nil) ; arguments
      ("PyTuple_GetItem"    :borrowed)
      ("PyErr_SetString"    nil)
      ("PyErr_Fetch"        nil)
      ("PyImport_AddModule" :borrowed)
      ("PyImport_ImportModule" :new)
      ("PyModule_GetDict"   :borrowed)
      ("PyErr_Occurred"     :borrowed)
      ("PyLong_AsSsize_t"   nil)
      ("PyTypeObject_Name"  nil)
      ("PyArray_Descr_from_element_type_code" nil)
      ("PyArray_NewFromDescr"  :new) ; arguments - steals a reference
      ("PyArray_DescrFromType" :new)
      ("PyArray_Data"       nil)
      ("memcpy"             nil)
      ("PyArray_element_type_from_array" nil))
  :test #'equal)

(defmacro pyforeign-funcall (name-and-options &rest args)
  "Wraps CFFI:FOREIGN-FUNCALL in WITH-PYTHON-GIL

Handles the reference counting of the return values but not the arguments."
  (let ((name (etypecase name-and-options
                (string name-and-options)
                (cons (first name-and-options)))))
    (with-gensyms (ptr)
      `(let ((,ptr (with-python-gil
                     (foreign-funcall ,name-and-options ,@args))))
         ,(case (progn
                  (assert (assoc name +python-function-reference-type-alist+
                                 :test #'string=))
                  (first (assoc-value +python-function-reference-type-alist+ name
                                      :test #'string=)))
            (:new      `(pytrack ,ptr))
            (:stolen   `(with-python-gil (foreign-funcall "Py_IncRef" :pointer ,ptr)))
            (:borrowed `()))
         ,ptr))))

;;; Reference counting utilities

(defvar *python-new-references* (make-hash-table))

(setf (documentation '*python-new-references* 'variable)
      "A foreign object returned as a result of python C API function that
returns a new reference should call PYTRACK.

PYGC will then decrement the references when called.")

(defvar *top-level-p* t
  "Used inside PYGC and WITH-PYGC to avoid calling PYGC at non-top levels.
This avoids inadvertent calls to DecRef during recursions.")

(defun pygc ()
  (when *top-level-p*
    (maphash-keys (lambda (key)
                    (let ((count (gethash key *python-new-references*))
                          (ptr (make-pointer key)))
                      ;; (dotimes (i (print count))
                      ;;   (pyforeign-funcall "Py_DecRef" :pointer ptr))
                      ;; (format t "~&DecRef-ing ~D~%" key)
                      (pyforeign-funcall "Py_DecRef" :pointer ptr))
                    (remhash key *python-new-references*))
                  *python-new-references*))
  nil)

(defmacro with-pygc (&body body)
  "Code that expects to return a pointer for later use should not be surrounded by PYGC"
  `(unwind-protect
        (let ((*top-level-p* nil))
          ,@body)
     ;; FIXME: When to call CLEAR-LISP-OBJECTS
     ;; (print (hash-table-alist *python-new-references*))
     (pygc)))


(defun pytrack (python-object-pointer)
  "Call this function when the foreign function of the Python C-API returns
a New Reference. The value in *PYTHON-NEW-REFERENCES* indicates the number
of new references owned by lisp."
  (declare (type foreign-pointer python-object-pointer)
           (optimize speed))
  (unless (null-pointer-p python-object-pointer)
    (when (boundp '*python-new-references*)
      (let ((ht   *python-new-references*)
            (addr (pointer-address python-object-pointer)))
        (unless (gethash addr ht)
          (setf (gethash addr ht) 0))
        (incf (gethash addr ht)))))
  python-object-pointer)


(defun pyuntrack (python-object-pointer)
  "Call this function when the foreign function of the Python C-API steals
a reference. The value in *PYTHON-NEW-REFERENCES* indicates the number
of new references owned by lisp."
  (declare (type foreign-pointer python-object-pointer)
           (optimize speed))
  (when (boundp '*python-new-references*)
    (let ((ht   *python-new-references*)
          (addr (pointer-address python-object-pointer)))
      ;; FIXME: Should we remove or should we decrement?
      (if (gethash addr ht)
          (remhash addr ht)
          ;; If we do not own any references, and yet someone
          ;; wants to UNTRACK the object, that means perhaps they
          ;; are stealing the reference, and we better increment the reference.
          (pyforeign-funcall "Py_IncRef" :pointer python-object-pointer))))
  nil)

(defun pyobject-tracked-p (python-object-pointer)
  "Call this function when the foreign function of the Python C-API steals
a New Reference"
  (declare (type foreign-pointer python-object-pointer)
           (optimize speed))
  (if (boundp '*python-new-references*)
      (values (let ((ht   *python-new-references*)
                    (addr (pointer-address python-object-pointer)))
                (if (gethash addr ht)
                    t
                    nil))
              t)
      (values nil nil)))
