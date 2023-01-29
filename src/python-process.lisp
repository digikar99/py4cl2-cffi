(in-package :py4cl2-cffi)

;; Basic Reference: https://www.linuxjournal.com/article/8497
;; Multithreading reference: https://www.linuxjournal.com/article/3641

(defvar *python-libraries-loaded-p* nil)
(defvar *in-with-remote-objects-p* nil)

(defun load-python-and-libraries ()
  (load-foreign-library *python-shared-object-path*)
  (dolist (lib (loop :for lib :in *python-additional-libraries*
                     :nconcing
                     (uiop:directory-files
                      *python-additional-libraries-search-path* (format nil "lib~A.so*" lib))))
    (load-foreign-library lib))
  (load-foreign-library *utils-shared-object-path*)
  (setq *python-libraries-loaded-p* t))

(declaim (inline python-alive-p python-start-if-not-alive))
(defun python-alive-p ()
  (unless *python-libraries-loaded-p*
    (load-python-and-libraries))
  (/= 0 (pyforeign-funcall "Py_IsInitialized" :int)))

(defun python-start-if-not-alive ()
  (unless (python-alive-p) (pystart)))

(defvar *py-module-pointer-table* (make-hash-table :test #'equal)
  "Key: A string indicating module name
Value: The pointer to the module in embedded python")
(declaim (type hash-table *py-module-pointer-table*))

(defun py-module-pointer (name)
  (declare (type string name))
  (or (nth-value 0 (gethash name *py-module-pointer-table*))
      (progn
        (python-start-if-not-alive)
        (setf (py-module-pointer name)
              (pyforeign-funcall "PyImport_AddModule" :string name :pointer)))))

(defun (setf py-module-pointer) (module-pointer name)
  (declare (type string name))
  (setf (gethash name *py-module-pointer-table*) module-pointer))

(defun py-module-dict (name)
  (declare (type string name))
  (pyforeign-funcall "PyModule_GetDict"
                     :pointer (py-module-pointer name)
                     :pointer))

(defvar *py-global-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the global namespace of embedded python.")
(defvar *py-builtins-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the builtins namespace of embedded python.")

;;; This is more of a global variable than a dynamic variable.
(defvar *in-with-python-output* nil)

(defvar *py-output-stream* nil)
(defvar *py-output-reader-thread* nil)
(defvar *python-output-semaphore* (bt:make-semaphore))
(defvar *py-error-output-stream* nil)
(defvar *py-error-output-reader-thread* nil)

;; TODO: Implement the with-python-output equivalent
(defun python-output-thread ()
  (when (and *py-output-reader-thread*
             (bt:thread-alive-p *py-output-reader-thread*))
    (bt:destroy-thread *py-output-reader-thread*))
  (setq *py-output-reader-thread*
        (bt:make-thread (lambda ()
                          (let ((py-out *py-output-stream*))
                            (iter outer
                              (for char =
                                   (progn
                                     ;; PEEK-CHAR waits for input
                                     (peek-char nil py-out nil)
                                     (when *in-with-python-output*
                                       (iter (while *in-with-python-output*)
                                         (bt:wait-on-semaphore *python-output-semaphore*))
                                       (in outer (next-iteration)))
                                     (read-char py-out nil)))
                              (when char (write-char char) *standard-output*))))))
  (when (and *py-error-output-reader-thread*
             (bt:thread-alive-p *py-error-output-reader-thread*))
    (bt:destroy-thread *py-error-output-reader-thread*))
  (setq *py-error-output-reader-thread*
        (bt:make-thread (lambda ()
                          ;; PEEK-CHAR waits for input
                          (loop :do (peek-char nil *py-error-output-stream* nil)
                                    (write-char (read-char *py-error-output-stream* nil)
                                                *error-output*))))))

(defmacro with-python-output (&body forms-decl)
  "Gets the output of the python program executed in FORMS-DECL in the form a string."
  `(with-output-to-string (output-stream)
     (when (and *warn-on-unavailable-feature-usage*
                (not (member :with-python-output *internal-features*)))
       (warn "WITH-PYTHON-OUTPUT may not work on your system."))
     (unwind-protect (progn
                       (setq *in-with-python-output* t)
                       ,@forms-decl
                       (let ((py-out *py-output-stream*))
                         (pycall "sys.stdout.flush")
                         (iter (while (listen py-out))
                           (for char = (read-char py-out nil))
                           (when char (write-char char output-stream)))))
       (setq *in-with-python-output* nil)
       (bt:signal-semaphore *python-output-semaphore*))))

(defun pystart ()

  (uiop:delete-file-if-exists #P"/tmp/py4cl2-cffi-output")
  (uiop:run-program "mkfifo /tmp/py4cl2-cffi-output" :output t :error-output *error-output*)
  (uiop:delete-file-if-exists #P"/tmp/py4cl2-cffi-error-output")
  (uiop:run-program "mkfifo /tmp/py4cl2-cffi-error-output" :output t :error-output *error-output*)

  (load-python-and-libraries)
  (foreign-funcall "Py_Initialize")
  (float-features:with-float-traps-masked (:overflow)
    (when (numpy-installed-p)
      (pushnew :typed-arrays *internal-features*))
    (when (member :typed-arrays *internal-features*)
      (setq *numpy-c-api-pointer*
            (foreign-funcall "import_numpy" :pointer))))
  (import-module "sys")
  (import-module "traceback")
  (import-module "fractions")
  (raw-pyexec "from fractions import Fraction")
  (let ((python-output-reader-open-thread
          ;; Need to do this in a separate initialization thread to deal with
          ;; blocking 'open' for named pipes
          (bt:make-thread
           (lambda ()
             (setq *py-output-stream* (open "/tmp/py4cl2-cffi-output" :direction :input))))))
    (raw-pyexec "sys.stdout = open('/tmp/py4cl2-cffi-output', 'w')")
    (bt:join-thread python-output-reader-open-thread))
  (let ((python-error-output-reader-open-thread
          (bt:make-thread
           (lambda ()
             (setq *py-error-output-stream*
                   (open #P"/tmp/py4cl2-cffi-error-output" :direction :input))))))
    (raw-pyexec "sys.stderr = open('/tmp/py4cl2-cffi-error-output', 'w')")
    (bt:join-thread python-error-output-reader-open-thread))
  (python-output-thread)

  (dolist (mod '("__main__" "builtins" "sys"))
    (setf (py-module-pointer mod)
          (pyforeign-funcall "PyImport_AddModule" :string mod :pointer)))
  (setq *py-global-dict*
        (pyforeign-funcall "PyModule_GetDict" :pointer (py-module-pointer "__main__") :pointer))
  (setq *py-builtins-dict*
        (pyforeign-funcall "PyModule_GetDict" :pointer (py-module-pointer "builtins") :pointer))

  (foreign-funcall "set_lisp_callback_fn_ptr" :pointer (callback lisp-callback-fn))
  (raw-pyexec #.(format nil "
import ctypes
py4cl_utils = ctypes.cdll.LoadLibrary(\"~A\")

class _py4cl_LispCallbackObject (object):
    \"\"\"
    Represents a lisp function which can be called.

    An object is used rather than a lambda, so that the lifetime
    can be monitoried, and the function removed from a hash map
    \"\"\"
    lisp_callback_fn = getattr(py4cl_utils, \"LispCallback_helper\")
    lisp_callback_fn.restype = ctypes.py_object
    def __init__(self, handle):
        \"\"\"
        handle    A number, used to refer to the object in Lisp
        \"\"\"
        self.handle = handle
    def __call__(self, *args, **kwargs):
        if args is None: args = tuple()
        if kwargs is None: kwargs = dict()
        return _py4cl_LispCallbackObject.lisp_callback_fn(
          ctypes.c_int(self.handle),
          ctypes.py_object(args),
          ctypes.py_object(kwargs)
        )
" (namestring *utils-shared-object-path*)))
  (cond ((and (numpy-installed-p)
              (not (member :arrays *internal-features*)))
         (push :arrays *internal-features*))
        ((and (not (numpy-installed-p))
              (member :arrays *internal-features*))
         (removef *internal-features* :arrays)))
  t)

(define-condition pyerror (error)
  ((format-control   :initarg :format-control
                     :initform "A python error occured")
   (format-arguments :initarg :format-arguments
                     :initform ()))
  (:report (lambda (condition stream)
             (with-slots (format-control format-arguments)
                 condition
               (apply #'format stream format-control format-arguments)))))

(defvar *retrieving-exceptions-p* nil
  "Set to non-NIL inside PYTHON-MAY-BE-ERROR to avoid infinite recursion.")

(defun python-may-be-error ()
  (declare (optimize debug))
  (python-start-if-not-alive)
  (let ((may-be-error-type (pyforeign-funcall "PyErr_Occurred" :pointer))
        (*retrieving-exceptions-p* t))
    (unless (null-pointer-p may-be-error-type)
      (with-pygc
        (with-foreign-objects ((ptype  :pointer)
                               (pvalue :pointer)
                               (ptraceback :pointer))
          (pyforeign-funcall "PyErr_Fetch"
                             :pointer ptype
                             :pointer pvalue
                             :pointer ptraceback)
          (let* ((type      (mem-aref ptype :pointer))
                 (value     (mem-aref pvalue :pointer))
                 (traceback (mem-aref ptraceback :pointer))
                 (value-str
                   (foreign-string-to-lisp
                    (pyforeign-funcall "PyUnicode_AsUTF8"
                                       :pointer (pyforeign-funcall "PyObject_Str"
                                                                   :pointer value
                                                                   :pointer)
                                       :pointer)))
                 (traceback-str
                   (pycall "traceback.format_exception" type value traceback)))
            (if traceback-str
                (error 'pyerror
                       :format-control "A python error occurred:~%  ~A~%~%Traceback:~%~%~A"
                       :format-arguments (list value-str traceback-str))
                (error 'pyerror
                       :format-control "A python error occurred:~%  ~A"
                       :format-arguments (list value-str)))))))))

(defmacro with-python-exceptions (&body body)
  (with-gensyms (may-be-exception-type)
    `(progn
       (unless *retrieving-exceptions-p* (python-may-be-error))
       (locally ,@body))))

(defmacro ensure-non-null-pointer (pointer
                                   &key (format-control nil format-control-p)
                                     format-arguments)
  (once-only (pointer)
    `(if (null-pointer-p ,pointer)
         ,(if format-control-p
              `(error 'pyerror
                      :format-control ,format-control
                      :format-arguments ,format-arguments)
              `(error 'pyerror))
         ,pointer)))

(defun raw-py (cmd-char &rest code-strings)
  "CMD-CHAR should be #\e for eval and #\x for exec.

Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (python-start-if-not-alive)
  (unless (zerop (pyforeign-funcall "PyRun_SimpleString"
                                    :string (apply #'concatenate
                                                   'string
                                                   (ecase cmd-char
                                                     (#\e "_ = ")
                                                     (#\x ""))
                                                   code-strings)
                                    :int))
    (python-may-be-error))
  (ecase cmd-char
    (#\e (pyvalue "_"))
    (#\x (values))))

(defun raw-pyeval (&rest code-strings)
  "
Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (with-pygc (apply #'raw-py #\e code-strings)))

(defun raw-pyexec (&rest code-strings)
  "
Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (apply #'raw-py #\x code-strings))

(defun pystop ()
  (setq *py-module-pointer-table* (make-hash-table :test #'equal))
  (setq *py-global-dict* nil)
  (setq *py-builtins-dict* nil)
  (when (python-alive-p)
    (raw-py #\e "sys.stdout.close()")
    (raw-py #\e "sys.stderr.close()")
    ;; It is okay to call Py_FinalizeEx even when python is not initialized
    (let ((value (pyforeign-funcall "Py_FinalizeEx" :int)))
      (cond ((zerop value)
             t)
            ((= -1 value)
             (error "-1 return value from Py_FinalizeEx indicating an error"))
            (t
             (error "Unexpected ~D return value from Py_FinalizeEx (expected 0 or -1)"
                    value))))))

(defun pytype (name)
  (python-start-if-not-alive)
  (with-foreign-string (name name)
    (pyforeign-funcall "PyDict_GetItemString"
                       :pointer *py-builtins-dict*
                       :pointer name
                       :pointer)))

(defun %pyvalue (python-value-or-variable)
  "Get the foreign pointer associated with PYTHON-VALUE-OR-VARIABLE.
The PYTHON-VALUE-OR-VARIABLE may not contain '.' (full-stops)
Use PYVALUE* if you want to refer to names containing full-stops."
  (declare (type (or foreign-pointer string) python-value-or-variable))
  (python-start-if-not-alive)
  (if (typep python-value-or-variable 'foreign-pointer)
      python-value-or-variable
      (let* ((name python-value-or-variable)
             ;; GetItemString returns a borrowed reference - neither stolen nor new
             (value (pyforeign-funcall "PyDict_GetItemString"
                                       :pointer (py-module-dict "__main__")
                                       :string name
                                       :pointer))
             (value (if (null-pointer-p value)
                        (pyforeign-funcall "PyDict_GetItemString"
                                           :pointer (py-module-dict "builtins")
                                           :string name
                                           :pointer)
                        value)))
        (ensure-non-null-pointer value
                                 :format-control "~A is undefined in python"
                                 :format-arguments (list name))
        value)))

(defun (setf %pyvalue) (new-value python-variable)
  "Sets the value of PYTHON-VARIABLE in the global namespace to NEW-VALUE"
  (declare (type string python-variable)
           (type foreign-pointer new-value))
  (python-start-if-not-alive)
  (pyforeign-funcall "PyDict_SetItemString"
                     :pointer (py-module-dict "__main__")
                     :string python-variable
                     :pointer new-value
                     :int)
  (python-may-be-error)
  new-value)

(defun %pyslot-value (object-pointer slot-name)
  (declare (type string slot-name)
           (type foreign-pointer object-pointer)
           (optimize debug))
  (python-start-if-not-alive)
  (let* ((return-value (pyforeign-funcall "PyObject_GetAttrString"
                                          :pointer object-pointer
                                          :string slot-name
                                          :pointer)))
    (ensure-non-null-pointer return-value
                             :format-control
                             "~A~%in python does not have the attribute ~A"
                             :format-arguments (list (pycall "str" object-pointer)
                                                     slot-name))))

(defun (setf %pyslot-value) (new-value object-pointer slot-name)
  (declare (type string slot-name)
           (type foreign-pointer object-pointer new-value)
           (optimize debug))
  (python-start-if-not-alive)
  (let* ((return-value (pyforeign-funcall "PyObject_SetAttrString"
                                          :pointer object-pointer
                                          :string slot-name
                                          :pointer new-value
                                          :int)))
    (if (zerop return-value)
        nil
        (python-may-be-error))))

(defun pyvalue* (python-value-or-variable)
  "Get the non-lispified value associated with PYTHON-VALUE-OR-VARIABLE"
  (declare (type (or python-object string) python-value-or-variable))
  (if (python-object-p python-value-or-variable)
      python-value-or-variable
      (let* ((names (split-sequence:split-sequence #\.
                                                   python-value-or-variable)))
        (loop :for name :in (rest names)
              :with value := (%pyvalue (first names))
              :do (setq value (%pyslot-value value name))
              :finally (return value)))))

(defun (setf pyvalue*) (new-value python-value-or-variable)
  (declare (type (or python-object string) python-value-or-variable)
           (type foreign-pointer new-value))
  (python-start-if-not-alive)
  (if (python-object-p python-value-or-variable)
      python-value-or-variable
      (let* ((names (split-sequence:split-sequence #\.
                                                   python-value-or-variable)))
        (if (endp (rest names))
            (setf (%pyvalue (first names)) new-value)
            (loop :for (name . rest) :on (rest names)
                  :with value := (%pyvalue (first names))
                  :if (endp rest)
                    :do (setf (%pyslot-value value name) new-value)
                  :else
                    :do (setq value (%pyslot-value value name)))))))

(defun pyvalue (python-value-or-variable)
  (declare (type (or python-object string) python-value-or-variable))
  (if *in-with-remote-objects-p*
      (pyvalue* python-value-or-variable)
      (with-pygc (lispify (pyvalue* python-value-or-variable)))))

(defun (setf pyvalue) (new-value python-value-or-variable)
  (declare (type string python-value-or-variable))
  (with-pygc
    (setf (pyvalue* python-value-or-variable) (%pythonize new-value))
    new-value))
