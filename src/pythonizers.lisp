(in-package :py4cl2-cffi)

(defvar *pyobject-translation-mode* :lisp)
(declaim (type (member :foreign-pointer :wrapper :lisp)
               *pyobject-translation-mode*))
(defstruct pyobject-wrapper
  "A wrapper around a pointer to a python object.
LOAD-FORM is used if the pyobject-wrapper is dumped into a compiled lisp file."
  pointer
  load-form)

(defun make-tracked-pyobject-wrapper (pointer)
  (let* ((pyobject-wrapper (make-pyobject-wrapper :pointer pointer)))
    (unless (null-pointer-p pointer)
      ;; Because this is a freshly created pyobject-wrapper,
      ;; it cannot have any other additional finalizers.
      (tg:finalize pyobject-wrapper
                   (finalization-lambda (pointer-address pointer))))
    pyobject-wrapper))

(defun finalization-lambda (address)
  (lambda ()
    (unless (zerop address)
      (with-python-gil/no-errors
        (foreign-funcall "Py_DecRef" :pointer (make-pointer address))))))

(defun pytrack* (pyobject-wrapper)
  "Call this function when the foreign function of the Python C-API returns
a New Reference"
  (declare (type pyobject-wrapper pyobject-wrapper)
           (optimize speed))
  (let ((ptr (pyobject-wrapper-pointer pyobject-wrapper)))
    (unless (null-pointer-p ptr)
      (tg:finalize pyobject-wrapper (finalization-lambda (pointer-address ptr)))))
  pyobject-wrapper)

(defun pyuntrack* (pyobject-wrapper)
  "Call this function when the foreign function of the Python C-API steals
a New Reference"
  (declare (type pyobject-wrapper pyobject-wrapper)
           (optimize speed))
  (tg:cancel-finalization pyobject-wrapper)
  nil)

(defvar *print-pyobject* t
  "If non-NIL, python's 'str' is called on the python-object before printing.")

(defvar *print-pyobject-wrapper-identity* t
  "If non-NIL, print's the lisp type and identity of the pyobject-wrapper.")

(defun pyobject-wrapper-eq (o1 o2)
  "Returns T if O1 and O2 are both PYOBJECT-WRAPPER with the same pointer, or
the same lisp objects which are EQ to each other. Returns NIL in all other cases."
  (or (eq o1 o2)
      (and (typep o1 'pyobject-wrapper)
           (typep o2 'pyobject-wrapper)
           (pyobject-wrapper-eq* o1 o2))))

(declaim (inline pyobject-wrapper-eq*))
(defun pyobject-wrapper-eq* (o1 o2)
  "Like PYOBJECT-WRAPPER-EQ but assumes that O1 and O2 are PYOBJECT-WRAPPER each."
  (declare (type pyobject-wrapper o1 o2)
           (optimize speed))
  (pointer-eq (pyobject-wrapper-pointer o1)
              (pyobject-wrapper-pointer o2)))

(defmethod print-object ((o pyobject-wrapper) s)
  (with-pygc
    (float-features:with-float-traps-masked t
      (let* ((pointer (pyobject-wrapper-pointer o))
             (type (let ((may-be-type (pyforeign-funcall "PyObject_Type"
                                                         :pointer pointer
                                                         :pointer)))
                     (ensure-non-null-pointer may-be-type)
                     (lispify
                      (pyforeign-funcall "PyObject_Str"
                                         :pointer may-be-type
                                         :pointer)))))
        (if *print-pyobject-wrapper-identity*
            (print-unreadable-object (o s :type t :identity t)
              (if *print-pyobject*
                  (progn
                    (format s ":type ~A~%" type)
                    (pprint-logical-block (s nil :per-line-prefix "  ")
                      (format s (if (string= "<class 'str'>" type)
                                    "\"~A\""
                                    "~A")
                              (lispify (pyforeign-funcall "PyObject_Str"
                                                          :pointer pointer
                                                          :pointer))))
                    (terpri s))
                  (format s ":POINTER ~A :TYPE ~A" pointer type)))
            (if *print-pyobject*
                (format s (if (string= "<class 'str'>" type)
                              "\"~A\""
                              "~A")
                        (lispify (pyforeign-funcall "PyObject_Str"
                                                    :pointer pointer
                                                    :pointer)))
                (format s ":POINTER ~A :TYPE ~A" pointer type)))))))

(defvar +py-empty-tuple+)
(defvar +py-empty-tuple-pointer+)
(defvar +py-none+)
(defvar +py-none-pointer+)

(defmethod make-load-form ((o pyobject-wrapper) &optional env)
  (declare (ignore env))
  (with-slots (pointer load-form) o
    (cond ((eq pointer +py-empty-tuple-pointer+)
           `(pycall "tuple"))
          ((eq pointer +py-none-pointer+)
           `(pyvalue "None"))
          (load-form
           load-form)
          (t
           `(pyeval ,(pycall "repr" pointer))))))

(declaim (type (function (t) foreign-pointer) pythonize))
(defgeneric pythonize (lisp-value-or-object)
  (:documentation "Given a lisp object, return a CFFI:FOREIGN-POINTER pointing to the python object corresponding to the given lisp object.

The implemented methods are expected to return a new (strong) reference
to the python object. The method is also expected to call PYTRACK
to notify the PYGC functionality to delete the reference once the object
is no longer needed.

See the documentation for PYGC to understand when reference deletion
takes place."))

(defmethod pythonize (lisp-object)
  (pycall* "_py4cl_UnknownLispObject" (object-handle lisp-object)))

(deftype c-long ()
  (let ((num-bits (* 8 (cffi:foreign-type-size :long))))
    `(signed-byte ,num-bits)))

(defmethod pythonize ((o #+sbcl sb-sys:system-area-pointer
                         #+ccl  ccl:macptr
                         #+ecl  si:foreign-data
                         #+lispworks fli::pointer
                         #-(or sbcl ccl ecl lispworks)
                         foreign-pointer))
  o)
(defmethod pythonize ((o pyobject-wrapper)) (pyobject-wrapper-pointer o))

(defmethod pythonize ((o integer))
  (unless (typep o 'c-long)
    ;; TODO: Proper warning class
    (warn "Given integer ~S is too big to be interpreted as a C long" o))
  (pyforeign-funcall "PyLong_FromLong" :long o :pointer))

(defmethod pythonize ((o float))
  ;; TODO: Different numpy float types: float32 and float64
  (pyforeign-funcall "PyFloat_FromDouble"
                     :double (coerce o 'double-float)
                     :pointer))

(defmethod pythonize ((o complex))
  ;; TODO: Different numpy float types: float32 and float64
  (pyforeign-funcall "PyComplex_FromDoubles"
                     :double (coerce (realpart o) 'double-float)
                     :double (coerce (imagpart o) 'double-float)
                     :pointer))

;; Use PYVALUE and friends instead of using PYTHONIZE for these.

(defmethod pythonize ((o string))
  (pyforeign-funcall "PyUnicode_FromString" :string o :pointer))

(defmethod pythonize ((o list))
  (pythonize-list o))

(defmethod pythonize ((o vector))
  (if (typep o '(vector t))
      (let ((list (pyforeign-funcall "PyList_New" :int (length o) :pointer)))
        (loop :for elt :across o
              :for pyelt := (pythonize elt)
              :for pos :from 0
              :do (if (zerop (pyforeign-funcall "PyList_SetItem"
                                                :pointer list
                                                :int pos
                                                :pointer pyelt
                                                :int))
                      (pyuntrack pyelt)
                      (python-may-be-error)))
        list)
      (pythonize-array o)))

(defmethod pythonize ((o hash-table))
  (let ((dict (pyforeign-funcall "PyDict_New" :pointer)))
    (maphash (lambda (key value)
               (let ((key   (pythonize key))
                     (value (pythonize value)))
                 (if (zerop (pyforeign-funcall "PyDict_SetItem"
                                               :pointer dict
                                               :pointer key
                                               :pointer value
                                               :int))
                     nil ;; No reference stealing
                     (python-may-be-error))))
             o)
    dict))

(defmethod pythonize ((o array))
  (pythonize-array o))

(defcvar ("getattr_ptr" *getattr-ptr*) :pointer)
(defcallback getattr-fn :pointer ((handle :int) (attr :pointer))
  (handler-case
      (pythonize (python-getattr (lisp-object handle) (lispify attr)))
    (error (c)
      (pyforeign-funcall "PyErr_SetString"
                         :pointer (pytype "Exception")
                         :string (format nil "An error occured during lisp callback.~%~%~A~%" (trivial-backtrace:print-backtrace c :output nil)))
      (null-pointer))))

(defcvar ("setattr_ptr" *setattr-ptr*) :pointer)
(defcallback setattr-fn :pointer ((handle :int) (attr :pointer) (new-value :pointer))
  (handler-case
      (python-setattr (lisp-object handle) (lispify attr) (lispify new-value))
    (error (c)
      (pyforeign-funcall "PyErr_SetString"
                         :pointer (pytype "Exception")
                         :string (format nil "An error occured during lisp callback.~%~%~A~%" (trivial-backtrace:print-backtrace c :output nil)))))
  (null-pointer))

(defcvar ("lisp_callback_fn_ptr" *lisp-callback-fn-ptr*) :pointer)
(defcallback lisp-callback-fn :pointer ((handle :int) (args :pointer) (kwargs :pointer))
  (declare (optimize debug))
  (with-pygc
    (thread-global-let ((*pyobject-translation-mode* :lisp))
      (handler-case
          (let ((lisp-callback (lisp-object handle)))
            (pythonize (apply lisp-callback
                              (nconc (let ((lispified-args
                                             (unless (null-pointer-p args)
                                               (lispify args))))
                                       (if (listp lispified-args)
                                           lispified-args
                                           nil))
                                     (unless (null-pointer-p kwargs)
                                       (loop :for i :from 0
                                             :for elt
                                               :in (hash-table-plist
                                                    (lispify kwargs))
                                             :collect (if (evenp i)
                                                          (intern (lispify-name elt) :keyword)
                                                          elt)))))))
        (error (c)
          (pyforeign-funcall "PyErr_SetString"
                             :pointer (pytype "Exception")
                             :string (format nil "An error occured during lisp callback.~%~%~A~%" (trivial-backtrace:print-backtrace c :output nil)))
          (pythonize 0))))))

(defmethod pythonize ((o function))
  (pycall* "_py4cl_LispCallbackObject" (object-handle o)))

(defstruct (%python-keyword (:conc-name nil)
                            (:constructor make-python-keyword (py-name)))
  (py-name nil :type string  :read-only t))
(declaim (inline make-python-keyword))
(deftype python-keyword () '(or %python-keyword keyword))
(defun python-keyword-p (object) (typep object 'python-keyword))

(defmethod pythonize ((o %python-keyword)) (pythonize (py-name o)))

(defun pythonize-array (array)
  (pytrack
   (let* ((descr        (pyforeign-funcall "PyArray_Descr_from_element_type_code"
                                           :string (array-element-typecode array)
                                           :pointer))
          (ndarray-type (pyforeign-funcall "PyDict_GetItemString"
                                           :pointer (py-module-dict "numpy")
                                           :string "ndarray"
                                           :pointer))
          (ndims        (array-rank array)))
     (with-foreign-objects ((dims    :long ndims))
       (dotimes (i ndims)
         (setf (mem-aref dims :long i) (array-dimension array i)))
       (if (type= t (array-element-type array))
           (let* ((numpy-array
                    (numpy-funcall "PyArray_NewFromDescr"
                                   :pointer ndarray-type
                                   :pointer descr
                                   :int ndims
                                   :pointer dims
                                   :pointer (null-pointer)
                                   :pointer (null-pointer)
                                   :int 0 ; non-zero flag indicates a fortran-style array
                                   :pointer (null-pointer)
                                   :pointer))
                  (array-data (pyforeign-funcall "PyArray_Data"
                                                 :pointer numpy-array :pointer)))
             (loop :for idx :below (array-total-size array)
                   :do (pyforeign-funcall "PyArray_SetItem"
                                          :pointer numpy-array
                                          :pointer (inc-pointer array-data (cl:* idx 8))
                                          :pointer (pythonize (row-major-aref array idx))
                                          :int))
             numpy-array)
           (with-pointer-to-vector-data (array-data (array-storage array))
             (incf-pointer array-data (* (cl-array-offset array)
                                         (array-element-type-num-bytes array)))
             (numpy-funcall "PyArray_NewFromDescr"
                            :pointer ndarray-type
                            :pointer descr
                            :int ndims
                            :pointer dims
                            :pointer (null-pointer)
                            :pointer array-data
                            :int (logior +npy-array-c-contiguous+ +npy-array-writeable+) ; flags
                            :pointer (null-pointer)
                            :pointer)))))))

(defun array-element-typecode (array)
  ;; This is necessary, because not all lisps using these specific names as the
  ;; element-types. Element-types returned by different lisps (eg: SBCL vs ECL)
  ;; would only be TYPE= to each other, and not STRING=
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
    ('(unsigned-byte 08) "ub8")
    ('t "t")))

(defun integer->py-size (int)
  (let ((len (pyforeign-funcall "PyLong_FromLong" :long int :pointer)))
    (pyforeign-funcall "PyLong_AsSsize_t" :pointer len :pointer)))

(defun py-size->integer (ptr)
  "WARNING: This function does not track reference counts."
  (with-python-gil/no-errors
    (let ((pylong (foreign-funcall "PyLong_FromSsize_t" :pointer ptr :pointer)))
      (foreign-funcall "PyLong_AsLong" :pointer pylong :long))))

(defun pythonize-list (list)
  (let* ((len   (length list))
         (tuple (pyforeign-funcall "PyTuple_New" :int len :pointer)))
    (assert (not (null-pointer-p tuple)))
    (loop :for elt :in list
          :for pyelt := (pythonize elt)
          :for pos :from 0
          :do (assert (zerop (pyforeign-funcall "PyTuple_SetItem"
                                                :pointer tuple
                                                :int pos
                                                :pointer pyelt
                                                :int)))
              (pyuntrack pyelt))
    tuple))

(defun pythonize-symbol (symbol)
  (if (null symbol)
      (pyvalue* "False")
      (let* ((symbol-name (symbol-name symbol))
             (name (cond ((and (char= (char symbol-name 0) #\*)
                               ;; *global-variable* == PYTHON_CONSTANT
                               (char= (char symbol-name (1- (length symbol-name)))))
                          (subseq symbol-name 1 (1- (length symbol-name))))
                         ((string= "T" symbol-name)
                          "True")
                         ((every #'(lambda (char) ; = every character is either upper-case
                                     (not (lower-case-p char))) ; or is not an alphabet
                                 symbol-name)
                          (format nil "~(~a~)" symbol-name))
                         (t
                          symbol-name))))
        ;; Replace - by _
        (iter (for char in-string name)
          (collect (if (char= char #\-)
                       #\_
                       char)
            into python-name
            result-type string)
          ;; Use keywords as if to indicate keyword python argument name
          (finally (return python-name))))))

(defmethod pythonize ((o symbol))
  (if (null o)
      (pyvalue* "False")
      (pythonize (pythonize-symbol o))))

(defun pythonize-plist (plist)
  (if (null plist)
      (null-pointer)
      (let ((dict (pyforeign-funcall "PyDict_New" :pointer)))
        (doplist (key val plist)
                 (assert (zerop (pyforeign-funcall "PyDict_SetItem"
                                                   :pointer dict
                                                   :pointer (pythonize key)
                                                   :pointer (pythonize val)
                                                   :int))))
        dict)))

(defmethod pythonize ((o rational))
  (pycall* "fractions.Fraction" (numerator o) (denominator o)))

(defvar *pythonizers*
  ()
  "Each entry in the alist *PYTHONIZERS* maps from a lisp-type to
a single-argument PYTHON-FUNCTION-DESIGNATOR. This python function takes as input the
\"default\" python objects and is expected to appropriately convert it to the corresponding
python object.

NOTE: This is a new feature and hence unstable; recommended to avoid in production code.")

(defmacro with-pythonizers ((&rest overriding-pythonizers) &body body)
  "Each entry of OVERRIDING-PYTHONIZERS is a two-element list of the form
  (TYPE PYTHONIZER)
Here, TYPE is unevaluated, while PYTHONIZER will be evaluated; the PYTHONIZER is expected
to take a default-pythonized object (see lisp-python types translation table in docs)
and return the appropriate object user expects.

For example,

  ; A convenience function
  (defun pyprint (object)
    (pycall \"print\" object)
    (pycall \"sys.stdout.flush\")
    (values))

  (pyprint #(1 2 3)) ; prints [1, 2, 3] ; the default object
  (with-pythonizers ((vector \"tuple\"))
    (pyprint #(1 2 3))
    (pyprint 5))
  ; (1, 2, 3) ; coerced to tuple by the pythonizer
  ; 5         ; pythonizer uncalled for non-VECTOR
  5

NOTE: This is a new feature and hence unstable; recommended to avoid in production code."
  `(let ((*pythonizers* (list* ,@(loop :for (type pythonizer) :in overriding-pythonizers
                                       :collect `(cons ',type ,pythonizer))
                               *pythonizers*)))
     ,@body))

(defun %pythonize (object)
  "A wrapper around PYTHONIZE to take custom *PYTHONIZERS* into account."
  (let ((default-pythonized-object (pythonize object)))
    (loop :for (type . pythonizer) :in *pythonizers*
          :if (typep object type)
            :do (return-from %pythonize (pycall* pythonizer default-pythonized-object)))
    default-pythonized-object))
