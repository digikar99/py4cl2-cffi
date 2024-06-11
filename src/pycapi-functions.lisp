(in-package :py4cl2-cffi)

(defmacro define-pycapi-function ((lisp-name cname) return-type &body args)
  (let ((lambda-list (mapcar #'first args)))
    `(progn
       (declaim (inline ,lisp-name))
       (defun ,lisp-name ,lambda-list
         (declare (optimize speed)
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (pyforeign-funcall ,cname ,@(apply #'append
                                            (mapcar #'reverse args))
                            ,return-type))
       (declaim (notinline ,lisp-name)))))

(define-pycapi-function (pyobject-call "PyObject_Call") :pointer
  (python-callable-pointer :pointer)
  (positional-args         :pointer)
  (keyword-args            :pointer))

(define-pycapi-function (pyobject-str "PyObject_Str") :pointer
  (python-object-pointer :pointer))
(define-pycapi-function (pyunicode-asutf8 "PyUnicode_AsUTF8") :pointer
  (pyunicode-object-pointer :pointer))

(define-pycapi-function (pyobject-type "PyObject_Type") :pointer
  (python-object-pointer :pointer))

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

(define-pycapi-function (pydict-getitem "PyDict_GetItem") :pointer
  (python-object-pointer :pointer)
  (item-pointer :pointer))

(define-pycapi-function (pydict-setitem "PyDict_SetItem") :pointer
  (python-object-pointer :pointer)
  (item-pointer :pointer)
  (value-pointer :pointer))

(define-pycapi-function (pyimport-addmodule "PyImport_AddModule") :pointer
  (name :string))
(define-pycapi-function (pyimport-importmodule "PyImport_ImportModule") :pointer
  (name :string))

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
