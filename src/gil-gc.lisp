(in-package :py4cl2-cffi)

(declaim (inline pygil-held-p pygil-release pygil-ensure
                 pyeval-save-thread pyeval-restore-thread))

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

(defvar *py-thread-state*)
(defun pyeval-save-thread ()
  (foreign-funcall "PyEval_SaveThread" :pointer))
(defun pyeval-restore-thread (thread-state)
  (foreign-funcall "PyEval_RestoreThread" :pointer thread-state))

(defvar *gil*)

(defmacro with-python-gil (&body body)
  "The intended usage of WITH-PYTHON-GIL to hold and release
the python gil without fail.

However, WITH-PYTHON-GIL can also be used at the toplevel to improve
the performance of python calls, as it avoids the need to
hold and release the python gil repeatedly. Note that doing so will
prevent other threads from running, including those calling
python-object finalizers during GC. But, this may not be a problem if
PYTHON-OBJECT are never generated.

For example, the following piece of code is expected to be faster

  (time
   (with-python-gil
     (loop for i below 100000
           do (pycall \"str\" i))))

than the following

  (time
   (loop for i below 100000
         do (pycall \"str\" i)))

"
  `(let* ((*gil* (pygil-ensure)))
     ;; A fair amount of consing comes from the dynamic binding of *GIL*
     ;; However, this is required, because WITHOUT-PYTHON-GIL below
     ;; accesses it. And this in-turn it utilized in READ-OUTPUT-WITH-SYNC
     (unwind-protect
          (unwind-protect (locally ,@body)
            (python-may-be-error))
       (pygil-release *gil*))))

(defmacro with-python-gil/no-errors (&body body)
  `(let* ((*gil* (pygil-ensure)))
     (unwind-protect
          (locally ,@body)
       (pygil-release *gil*))))

(defmacro without-python-gil (&body body)
  (with-gensyms (count)
    `(let ((,count (if (boundp '*gil*)
                       (loop :while (pygil-held-p)
                             :for ,count :from 0
                             :do (pygil-release *gil*)
                             :finally (return ,count))
                       0)))
       (unwind-protect (locally ,@body)
         (loop :repeat ,count
               :do (setq *gil* (pygil-ensure)))))))

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
      ("PyLong_FromSsize_t" :new)
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
      ("PyTuple_SetItem"    nil) ; arguments - steals a reference
      ("PyTuple_GetItem"    :borrowed)
      ("PyErr_SetString"    nil)
      ("PyErr_Fetch"        nil)
      ("PyImport_AddModule" :borrowed)
      ("PyImport_ImportModule" :new)
      ("PyModule_GetDict"   :borrowed)
      ("PyComplex_FromDoubles" :new)
      ("PyErr_Occurred"     :borrowed)
      ("PyLong_AsSsize_t"   nil)
      ("PyTypeObject_Name"  nil)
      ("PyArray_Descr_from_element_type_code" nil)
      ("PyArray_NewFromDescr"  :new) ; arguments - steals a reference
      ("PyArray_DescrFromType" :new)
      ("PyArray_Data"       nil)
      ("PyArray_GetItem"    nil)
      ("PyArray_SetItem"    nil)
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

;;; Object Handles - for not really translated lisp objects

(defvar *handle-counter* 0)
(defvar *lisp-objects* (make-hash-table :test #'eql))

(defun clear-lisp-objects ()
  "Clear the *lisp-objects* object store, allowing them to be GC'd"
  (maphash-keys (lambda (key) (remhash key *lisp-objects*)) *lisp-objects*)
  nil)

(defun lisp-object (handle)
  "Get the lisp object corresponding to HANDLE"
  (or (gethash handle *lisp-objects*)
      (error "Invalid Handle.")))

(defun object-handle (object)
  "Store OBJECT and return a handle"
  (let ((handle (incf *handle-counter*)))
    (setf (gethash handle *lisp-objects*) object)
    handle))

;;; Reference counting utilities

(defvar *python-new-references* (make-hash-table))

(setf (documentation '*python-new-references* 'variable)
      "A foreign object returned as a result of python C API function that
returns a new reference should call PYTRACK.

PYGC will then decrement the references when called.")

(defvar *top-level-p* t
  "Used inside PYGC and WITH-PYGC to avoid calling PYGC at non-top levels.
This avoids inadvertent calls to DecRef during recursions.")

;; FIXME: Do we leak this into the python-object finalizers?
(defvar *pygc-enabled* t
  "If NIL, expects PYGC to be called manually by the user.")

(defmacro enable-pygc ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *pygc-enabled* t)))

(defmacro disable-pygc ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *pygc-enabled* nil)))

(defun py-refcnt (python-object-pointer)
  (declare (type foreign-pointer python-object-pointer))
  (py-size->integer
   (foreign-funcall "Py_RefCnt"
                    :pointer python-object-pointer
                    :pointer)))

(declaim (inline pygc))
(defun pygc ()
  (declare (optimize speed))
  (when (and *top-level-p* *pygc-enabled*)
    (let ((ht *python-new-references*)
          (*top-level-p* nil))
      (declare (type hash-table ht))
      ;; (loop :for (addr . count) :in (hash-table-alist ht)
      ;;       :do (let ((ptr (make-pointer addr)))
      ;;             (format t "~%At ~A with refcnt ~A:~% ~A"
      ;;                     ptr count (lispify ptr))))
      (maphash (lambda (addr count)
                 (declare (type #+64-bit (unsigned-byte 64)
                                #-64-bit unsigned-byte
                                addr)
                          (type fixnum count))
                 (when (and addr (not (zerop addr)))
                   (with-python-gil/no-errors
                     (cond ((< count 0)
                            (foreign-funcall "Py_IncRef" :unsigned-long addr))
                           ((> count 0)
                            (foreign-funcall "Py_DecRef" :unsigned-long addr))))))
               ht)
      (maphash (lambda (addr count)
                 (declare (ignore count))
                 (remhash addr ht))
               ht))
    (clear-lisp-objects))
  nil)

(defmacro with-pygc (&body body)
  "Code surrounded by WITH-PYGC performs garbage collection
only after executing all of BODY."
  `(unwind-protect
        (let ((*top-level-p* nil))
          ,@body)
     (pygc)))


(defun pytrack (python-object-pointer)
  "Call this function when the foreign function of the Python C-API returns
a New Reference.

The value in *PYTHON-NEW-REFERENCES* indicates the number
of new references owned by lisp and which are not handled by finalizers
of PYTHON-OBJECT structure instance in lisp."
  (declare (type foreign-pointer python-object-pointer)
           (optimize speed))
  ;; (let ((*top-level-p* nil)
  ;;       (*already-retrieving-exceptions* t))
  ;;   (with-python-gil/no-errors
  ;;     (let ((pystr (foreign-funcall "PyObject_Str"
  ;;                                   :pointer python-object-pointer
  ;;                                   :pointer)))
  ;;       (print (list :track
  ;;                    python-object-pointer
  ;;                    (nth-value 0
  ;;                               (foreign-string-to-lisp
  ;;                                (foreign-funcall "PyUnicode_AsUTF8"
  ;;                                                 :pointer pystr
  ;;                                                 :pointer))))))))
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
a reference.

The value in *PYTHON-NEW-REFERENCES* indicates the number
of new references owned by lisp and which are not handled by finalizers
of PYTHON-OBJECT structure instance in lisp."
  (declare (type foreign-pointer python-object-pointer)
           (optimize speed))
  ;; (with-python-gil/no-errors
  ;;   (let ((pystr (foreign-funcall "PyObject_Str"
  ;;                                 :pointer python-object-pointer
  ;;                                 :pointer)))
  ;;     (print (list :untrack
  ;;                  python-object-pointer
  ;;                  (nth-value 0
  ;;                             (foreign-string-to-lisp
  ;;                              (foreign-funcall "PyUnicode_AsUTF8"
  ;;                                               :pointer pystr :pointer)))))))
  (unless (null-pointer-p python-object-pointer)
    (when (boundp '*python-new-references*)
      (let ((ht   *python-new-references*)
            (addr (pointer-address python-object-pointer)))
        ;; FIXME: Should we remove or should we decrement?
        (if (gethash addr ht)
            (remhash addr ht)
            ;; If lisp does not own any references, and yet someone
            ;; wants to UNTRACK the object, that means perhaps they
            ;; are stealing the reference, and we better increment the reference.
            (pyforeign-funcall "Py_IncRef" :pointer python-object-pointer)))))
  python-object-pointer)

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
