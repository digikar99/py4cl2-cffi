(in-package :py4cl2-cffi)

;;; Object Handles - for not really translated lisp objects

(defvar *handle-counter* 0)
(defvar *lisp-objects* (make-hash-table :test #'eql))

(defun clear-lisp-objects ()
  "Clear the *lisp-objects* object store, allowing them to be GC'd"
  (setf *lisp-objects* (make-hash-table :test #'eql)
        *handle-counter* 0))

(defun free-handle (handle)
  "Remove an object with HANDLE from the hash table"
  (remhash handle *lisp-objects*))

(defun lisp-object (handle)
  "Get the lisp object corresponding to HANDLE"
  (or (gethash handle *lisp-objects*)
      (error "Invalid Handle.")))

(defun object-handle (object)
  "Store OBJECT and return a handle"
  (let ((handle (incf *handle-counter*)))
    (setf (gethash handle *lisp-objects*) object)
    handle))

;;; Unknown python objects
(defstruct python-object
  "A pointer to a python object which couldn't be translated into a Lisp value.
TYPE slot is the python type string
POINTER slot points to the object"
  type
  pointer
  load-form)

(defun make-tracked-python-object (&key type pointer)
  (let* ((python-object (make-python-object :type type :pointer pointer)))
    (unless (null-pointer-p pointer)
      (tg:finalize python-object (finalization-lambda (pointer-address pointer))))
    python-object))

(defun finalization-lambda (address)
  (lambda ()
    (pyforeign-funcall "Py_DecRef" :pointer (make-pointer address))

    ;; (handler-case

    ;;     (sb-sys:memory-fault-error
    ;;      ()
    ;;      (format t "Memory fault error while DecRef-ing python object at ~A~%"
    ;;              address)
    ;;      (format t "Memory fault error while DecRef-ing python object~%  ~A~%  ~A"
    ;;              (foreign-string-to-lisp
    ;;               (pyforeign-funcall
    ;;                "PyUnicode_AsUTF8"
    ;;                :pointer (pyforeign-funcall "PyObject_Str"
    ;;                                            :pointer (make-pointer address) :pointer)
    ;;                :pointer))
    ;;              address)))
    ))

(defun pytrack* (python-object)
  "Call this function when the foreign function of the Python C-API returns
a New Reference"
  (declare (type python-object python-object)
           (optimize speed))
  (let ((ptr (python-object-pointer python-object)))
    (unless (null-pointer-p ptr)
      (tg:finalize python-object (finalization-lambda (pointer-address ptr)))))
  python-object)

(defun pyuntrack* (python-object)
  "Call this function when the foreign function of the Python C-API steals
a New Reference"
  (declare (type python-object python-object)
           (optimize speed))
  (tg:cancel-finalization python-object)
  nil)

(defvar *print-python-object* t
  "If non-NIL, python's 'str' is called on the python-object before printing.")

(defun python-object-eq (o1 o2)
  (declare (type python-object o1 o2))
  (pointer-eq (python-object-pointer o1)
              (python-object-pointer o2)))

(defmethod print-object ((o python-object) s)
  (print-unreadable-object (o s :type t :identity t)
    (with-pygc
      (with-slots (type pointer) o
        (if *print-python-object*
            (progn
              (format s ":type ~A~%" type)
              (pprint-logical-block (s nil :per-line-prefix "  ")
                ()
                (write-string (lispify (pyforeign-funcall "PyObject_Str"
                                                          :pointer pointer :pointer))
                              s))
              (terpri s))
            (format s ":POINTER ~A :TYPE ~A" pointer
                    (lispify (pyforeign-funcall "PyObject_Str"
                                                :pointer type :pointer))))))))

(defmethod make-load-form ((o python-object) &optional env)
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
(defgeneric pythonize (lisp-value-or-object))

(defmethod pythonize (lisp-object)
  (pycall* "_py4cl_UnknownLispObject"
           (type-of lisp-object)
           (object-handle lisp-object)))

(deftype c-long ()
  (let ((num-bits (* 8 (cffi:foreign-type-size :long))))
    `(signed-byte ,num-bits)))

(defmethod pythonize ((o #+sbcl sb-sys:system-area-pointer
                         #+ccl  ccl:macptr
                         #-(or sbcl ccl) foreign-pointer))
  o)
(defmethod pythonize ((o python-object)) (python-object-pointer o))

(defmethod pythonize ((o integer))
  (unless (typep o 'c-long)
    ;; TODO: Proper warning class
    (warn "Given integer ~S is too bit to be interpreted as a C long" o))
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

(defcallback free-handle-fn :pointer ((handle :int))
  (free-handle handle)
  (null-pointer))

(defcallback lisp-callback-fn :pointer ((handle :int) (args :pointer) (kwargs :pointer))
  (with-pygc
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
                           :string (format nil "~A" c))
        (pythonize 0)))))

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

  (raw-pyeval \"[1, 2, 3]\") ;=> #(1 2 3) ; the default object
  (with-pythonizers ((vector \"tuple\"))
    (print (pycall #'identity #(1 2 3)))
    (print (pycall #'identity 5)))
  ; (1 2 3)  ; coerced to tuple by the pythonizer, which then translates to list
  ; 5        ; pythonizer uncalled for non-VECTOR
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
