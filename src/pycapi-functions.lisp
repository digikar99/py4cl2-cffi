(in-package :py4cl2-cffi)

(defmacro define-pycapi-function ((lisp-name cname) return-type &body args)
  (let ((lambda-list (mapcar #'first args)))
    `(progn
       (declaim (inline ,lisp-name))
       (defun ,lisp-name ,lambda-list
         (declare (optimize speed)
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         ,(with-gensyms (return-value)
            `(let* ((,return-value)
                    (*gil* (pygil-ensure)))
               (unwind-protect
                    (let ((*pygil-toplevel-p* nil))
                      (unwind-protect
                           (setq ,return-value
                                 (foreign-funcall ,cname
                                                  ,@(apply #'append
                                                           (mapcar #'reverse args))
                                                  ,return-type))
                        (python-may-be-error))
                      ,(case (progn
                               (assert (assoc cname +python-function-reference-type-alist+
                                              :test #'string=))
                               (first (assoc-value +python-function-reference-type-alist+
                                                   cname
                                                   :test #'string=)))
                         (:new      `(pytrack ,return-value))
                         (:stolen   `(foreign-funcall "Py_IncRef" :pointer ,return-value))
                         (:borrowed `()))
                      ,return-value)
                 (pygil-release *gil*)))))
       (declaim (notinline ,lisp-name)))))

(define-pycapi-function (pyobject-call "PyObject_Call") :pointer
  (python-callable-pointer :pointer)
  (positional-args         :pointer)
  (keyword-args            :pointer))
(define-pycapi-function (pyobject-callobject "PyObject_CallObject") :pointer
  (python-callable-pointer :pointer)
  (positional-args         :pointer))

(define-pycapi-function (pyobject-str "PyObject_Str") :pointer
  (python-object-pointer :pointer))
(define-pycapi-function (pyunicode-asutf8 "PyUnicode_AsUTF8") :pointer
  (pyunicode-object-pointer :pointer))

(define-pycapi-function (pyobject-type "PyObject_Type") :pointer
  (python-object-pointer :pointer))
(define-pycapi-function (pytypeobject-name "PyTypeObject_Name") :pointer
  (python-type-object-pointer :pointer))
(define-pycapi-function (pyobject-typename "PyObject_TypeName") :pointer
  (python-object-pointer :pointer))

(declaim (inline pyobject-typename/simple))
(defun pyobject-typename/simple (python-object-pointer)
  (declare (optimize speed))
  (foreign-funcall "PyObject_TypeName" :pointer python-object-pointer :pointer))
(declaim (notinline pyobject-typename/simple))

(declaim (inline djb2-foreign-string-hash))
(defun djb2-foreign-string-hash (foreign-string)
  (declare (optimize speed))
  (foreign-funcall "djb2_strhash" :pointer foreign-string :unsigned-long))
(declaim (notinline djb2-foreign-string-hash))


(define-pycapi-function
    (pyobject-getattrstring "PyObject_GetAttrString")
    :pointer
  (python-object-pointer :pointer)
  (attr-string :string))

(define-pycapi-function
    (pyobject-setattrstring "PyObject_SetAttrString")
    :int
  (python-object-pointer :pointer)
  (attr-string :string)
  (attr-value-pointer :pointer))

(define-pycapi-function (pydict-new "PyDict_New") :pointer)
(define-pycapi-function (pydict-getitem "PyDict_GetItem") :pointer
  (python-object-pointer :pointer)
  (item-pointer :pointer))

(define-pycapi-function (pydict-setitem "PyDict_SetItem") :int
  (python-object-pointer :pointer)
  (item-pointer :pointer)
  (value-pointer :pointer))

(define-pycapi-function (pydict-getitemstring "PyDict_GetItemString") :pointer
  (python-dict-pointer :pointer)
  (item :string))


(define-pycapi-function (pytuple-new "PyTuple_New") :pointer
  (length :int))
(declaim (ftype (function (cffi:foreign-pointer) fixnum)))
(define-pycapi-function (pytuple-size "PyTuple_Size") :int
  (python-tuple-pointer :pointer))
(define-pycapi-function (pytuple-setitem "PyTuple_SetItem") :int
  (python-tuple-pointer :pointer)
  (position :int)
  (python-object-pointer :pointer))

(define-pycapi-function (pyimport-addmodule "PyImport_AddModule") :pointer
  (name :string))

(declaim (inline pyimport-importmodule))
(defun pyimport-importmodule (name)
  (declare (optimize speed)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((return-value) (*gil* (pygil-ensure)))
    (unwind-protect
         (let ((*pygil-toplevel-p* nil))
           (unwind-protect
                (setq return-value
                      (float-features:with-float-traps-masked t
                        ;; FIXME: We aren't quite sure if masking float traps is
                        ;; the right thing to do. But it certainly makes for more
                        ;; convenient user experience.
                        (foreign-funcall "PyImport_ImportModule" :string name
                                                                 :pointer)))
             (python-may-be-error))
           (pytrack return-value)
           return-value)
      (pygil-release *gil*))))
(declaim (notinline pyimport-importmodule))

(define-pycapi-function (pymodule-getdict "PyModule_GetDict") :pointer
  (python-module-pointer :pointer))


(define-pycapi-function (pyerr-occurred "PyErr_Occurred") :pointer)

(declaim (inline pyerr-occurred/simple))
(defun pyerr-occurred/simple ()
  (foreign-funcall "PyErr_Occurred" :pointer))

(define-pycapi-function (pyerr-fetch "PyErr_Fetch") :void
  (ptype :pointer)
  (pvalue :pointer)
  (ptraceback :pointer))

(defun pyerr-setstring/simple (type message)
  (declare (type foreign-pointer type)
           (type string message))
  (with-python-gil/no-errors
    (foreign-funcall "PyErr_SetString"
                     :pointer type
                     :string message
                     :void)))
