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

(defvar *python-state* :uninitialized)
(declaim (type (member :uninitialized :initialized :initializing) *python-state*))

(declaim (inline python-alive-p python-start-if-not-alive))
(defun python-alive-p () (eq :initialized *python-state*))

(defun python-start-if-not-alive ()
  (case *python-state*
    (:uninitialized (pystart))
    (t nil)))

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

(defvar *py-module-dict-pointer-table* (make-hash-table :test #'equal)
  "Key: A string indicating module name
Value: The pointer to the module dictionary in embedded python")
(declaim (type hash-table *py-module-dict-pointer-table*))
(defun py-module-dict (name)
  (declare (type string name)
           (optimize speed))
  (or (nth-value 0 (gethash name *py-module-dict-pointer-table*))
      (progn
        (python-start-if-not-alive)
        (setf (py-module-dict name)
              (pyforeign-funcall "PyModule_GetDict"
                                 :pointer (py-module-pointer name)
                                 :pointer)))))

(defun (setf py-module-dict) (module-dict-pointer name)
  (declare (type string name)
           (optimize speed))
  (setf (gethash name *py-module-dict-pointer-table*)
        module-dict-pointer))

(defvar *py-global-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the global namespace of embedded python.")
(defvar *py-builtins-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the builtins namespace of embedded python.")

;;; This is more of a global variable than a dynamic variable.
(defvar *in-with-python-output* nil)

(defvar *py-output-stream* nil)
(defvar *py-output-stream-string* nil)
(defvar *py-output-reader-thread* nil)
(defvar *py-error-output-stream* nil)
(defvar *py-error-output-reader-thread* nil)

(defvar *py-output-lock* (bt:make-lock "python-output-lock"))
(defvar *py-output-condition*
  (bt:make-condition-variable :name "python-output-condition"))

(let ((string-output-stream  nil))

  (defun python-output-thread ()
    (when (and *py-output-reader-thread*
               (bt:thread-alive-p *py-output-reader-thread*))
      (bt:destroy-thread *py-output-reader-thread*))
    (setq *py-output-reader-thread*
          (bt:make-thread
           (lambda ()
             (setq *py-output-stream* (open #P"/tmp/py4cl2-cffi-output" :direction :input))
             (loop :do (when (and *in-with-python-output*
                                  (not (listen *py-output-stream*)))
                         (bt:with-lock-held (*py-output-lock*)
                           (bt:condition-notify *py-output-condition*)))
                       ;; PEEK-CHAR waits for input
                       (peek-char nil *py-output-stream* nil)
                       (let ((char (read-char-no-hang *py-output-stream* nil)))
                         (when char
                           (write-char char
                                       (if *in-with-python-output*
                                           string-output-stream
                                           *standard-output*))))))))
    (when (and *py-error-output-reader-thread*
               (bt:thread-alive-p *py-error-output-reader-thread*))
      (bt:destroy-thread *py-error-output-reader-thread*))
    (setq *py-error-output-reader-thread*
          (bt:make-thread
           (lambda ()
             (setq *py-error-output-stream*
                   (open #P"/tmp/py4cl2-cffi-error-output" :direction :input))
             ;; PEEK-CHAR waits for input
             (loop :do (peek-char nil *py-error-output-stream* nil)
                       (let ((char (read-char *py-error-output-stream* nil)))
                         (when char
                           (write-char char *error-output*))))))))

  ;; TODO: Instead of having the thread write to the string-output-stream
  ;; open the named pipe and read from it directly.
  (defun call-thunk-python-output (thunk)
    "Capture and return the output produced by python during the
execution of THUNK as a string."
    (bt:with-lock-held (*py-output-lock*)
      (thread-global-let ((string-output-stream    (make-string-output-stream))
                          (*in-with-python-output* t))
        (funcall thunk)
        (pycall "sys.stdout.flush")
        (bt:condition-wait *py-output-condition* *py-output-lock*)
        (get-output-stream-string string-output-stream)))))

(defmacro with-python-output (&body forms-decl)
  "Gets the output of the python program executed in FORMS-DECL in the form a string."
  `(call-thunk-python-output (lambda () ,@forms-decl)))

(defun pystart ()

  (let ((*python-state* :initializing))

    ;; We are using a let, because if something fails, we want
    ;; *PYTHON-STATE* to be whatever it was before.

    (uiop:run-program
     "rm /tmp/py4cl2-cffi-output ; mkfifo /tmp/py4cl2-cffi-output"
     :output t :error-output *error-output*)
    (uiop:run-program
     "rm /tmp/py4cl2-cffi-error-output ; mkfifo /tmp/py4cl2-cffi-error-output"
                      :output t :error-output *error-output*)

    (load-python-and-libraries)
    (foreign-funcall "Py_Initialize")
    (when (pygil-held-p)
      (setq *py-thread-state* (pyeval-save-thread))
      (when (pygil-held-p)
        (warn "Python GIL was not released from the main thread. This means on implementations (like SBCL) that call lisp object finalizers from a separate thread may never get a chance to run, and thus python foreign objects associated with PYTHON-OBJECT
can lead to memory leak.")))
    (float-features:with-float-traps-masked (:overflow :invalid)
      (when (numpy-installed-p)
        (pushnew :typed-arrays *internal-features*))
      (when (member :typed-arrays *internal-features*)
        (setq *numpy-c-api-pointer*
              (with-python-gil (foreign-funcall "import_numpy" :pointer)))))
    (import-module "sys")
    (import-module "traceback")
    (import-module "fractions")
    (raw-pyexec "from fractions import Fraction")

    (python-output-thread)
    (raw-pyexec "sys.stdout = open('/tmp/py4cl2-cffi-output', 'w')")
    (raw-pyexec "sys.stderr = open('/tmp/py4cl2-cffi-error-output', 'w')")

    (dolist (mod '("__main__" "builtins" "sys"))
      (setf (py-module-pointer mod)
            (pyforeign-funcall "PyImport_AddModule" :string mod :pointer)))
    (setq *py-global-dict*
          (pyforeign-funcall "PyModule_GetDict" :pointer (py-module-pointer "__main__") :pointer))
    (setq *py-builtins-dict*
          (pyforeign-funcall "PyModule_GetDict" :pointer (py-module-pointer "builtins") :pointer))

    (foreign-funcall "set_lisp_callback_fn_ptr" :pointer (callback lisp-callback-fn))
    (foreign-funcall "set_helper_fn_ptr"
                     :pointer (callback getattr-fn)
                     :pointer (callback setattr-fn))
    (raw-pyexec #.(format nil "import ctypes
py4cl_utils = ctypes.cdll.LoadLibrary(\"~A\")
" (namestring *utils-shared-object-path*)))
    (raw-pyexec (read-file-into-string
                 (asdf:component-pathname
                  (asdf:find-component (asdf:find-system "py4cl2-cffi") "py4cl.py"))))
    (raw-pyexec "import decimal; Decimal = decimal.Decimal")
    (setq +py-empty-tuple-pointer+ (pycall* "tuple"))
    (setq +py-empty-tuple+ (pycall "tuple"))
    (setq +py-none-pointer+ (pyvalue* "None"))
    (setq +py-none+ (pyvalue "None"))
    (cond ((and (numpy-installed-p)
                (not (member :arrays *internal-features*)))
           (push :arrays *internal-features*))
          ((and (not (numpy-installed-p))
                (member :arrays *internal-features*))
           (removef *internal-features* :arrays))))
  (setq *python-state* :initialized)
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

(defun python-may-be-error (&optional already-retrieving-exceptions)
  (declare (optimize debug))
  (python-start-if-not-alive)
  (unless already-retrieving-exceptions
    (with-python-gil
      (let ((may-be-error-type (pyforeign-funcall "PyErr_Occurred" :pointer)))
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
                       (if (null-pointer-p traceback)
                           (pycall "traceback.format_exception_only" type value)
                           (pycall "traceback.format_exception" type value traceback))))
                (with-simple-restart (continue-ignoring-errors "")
                  (if traceback-str
                      (error 'pyerror
                             :format-control "A python error occurred:~%  ~A~%~%Traceback:~%~%~A"
                             :format-arguments (list value-str traceback-str))
                      (error 'pyerror
                             :format-control "A python error occurred:~%  ~A"
                             :format-arguments (list value-str))))))))))))

(defmacro with-python-exceptions (&body body)
  (with-gensyms (may-be-exception-type)
    `(progn
       (python-may-be-error t)
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
    (error 'pyerror
           :format-control "An unknown python error occurred.

Unfortunately, no more information about the error can be provided
while using RAW-PYEVAL or RAW-PYEXEC."))
  (ecase cmd-char
    (#\e (let ((ptr (pyvalue* "_")))
           (pyforeign-funcall "Py_IncRef" :pointer ptr)
           (pytrack ptr)))
    (#\x (values))))

(defun raw-pyeval (&rest code-strings)
  "
Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (if *in-with-remote-objects-p*
      (apply #'raw-py #\e code-strings)
      (with-pygc (lispify (apply #'raw-py #\e code-strings)))))

(defun raw-pyexec (&rest code-strings)
  "
Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (apply #'raw-py #\x code-strings))

(defun pystop ()
  (when (python-alive-p)
    (trivial-garbage:gc :full t)
    (pygc)
    (pycall (pyvalue* "sys.stdout.close"))
    (pycall (pyvalue* "sys.stderr.close"))
    (pymethod *py-global-dict* "clear")
    (setq *py-module-pointer-table* (make-hash-table :test #'equal))
    (setq *py-module-dict-pointer-table* (make-hash-table :test #'equal))
    (setq *py-global-dict* nil)
    (setq *py-builtins-dict* nil)
    (setq *python-state* :uninitialized)
    (makunbound '+empty-tuple+)
    (makunbound '+empty-tuple-pointer+)
    (makunbound '+py-none+)
    (makunbound '+py-none-pointer+)
    (bt:destroy-thread *py-output-reader-thread*)
    (bt:destroy-thread *py-error-output-reader-thread*)
    (sleep 0.01)
    nil))

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
  (declare (type (or foreign-pointer string) python-value-or-variable)
           (optimize speed))
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
  (declare (type (or symbol string) slot-name)
           (type foreign-pointer object-pointer)
           (optimize speed))
  (python-start-if-not-alive)
  (ensure-non-null-pointer object-pointer
                           :format-control
                           "Trying to access ~A slot of object with null pointer"
                           :format-arguments (list slot-name))
  (let* ((slot-name (etypecase slot-name
                      (string slot-name)
                      (symbol (pythonize-symbol slot-name))))
         (return-value (pyforeign-funcall "PyObject_GetAttrString"
                                            :pointer object-pointer
                                            :string slot-name
                                            :pointer)))
    (ensure-non-null-pointer return-value
                             :format-control
                             "~A~%in python does not have the attribute ~A"
                             :format-arguments (list (pycall "str" object-pointer)
                                                     slot-name))))

(defun (setf %pyslot-value) (new-value object-pointer slot-name)
  (declare (type (or string symbol) slot-name)
           (type foreign-pointer object-pointer new-value)
           (optimize speed))
  (python-start-if-not-alive)
  (let* ((slot-name (etypecase slot-name
                      (string slot-name)
                      (symbol (pythonize-symbol slot-name))))
         (return-value (pyforeign-funcall "PyObject_SetAttrString"
                                          :pointer object-pointer
                                          :string slot-name
                                          :pointer new-value
                                          :int)))
    (if (zerop return-value)
        nil
        (python-may-be-error))))

(defun pyvalue* (python-value-or-variable)
  "Get the non-lispified value associated with PYTHON-VALUE-OR-VARIABLE"
  (declare (type (or python-object string) python-value-or-variable)
           (optimize speed))
  (if (python-object-p python-value-or-variable)
      python-value-or-variable
      (let (value)
        (do-subseq-until (name python-value-or-variable #\. :test #'char=)
          (setq value (if value
                          (%pyslot-value value name)
                          (%pyvalue name))))
        value)))

(defun (setf pyvalue*) (new-value python-value-or-variable)
  (declare (type (or python-object string) python-value-or-variable)
           (type foreign-pointer new-value))
  (python-start-if-not-alive)
  (if (python-object-p python-value-or-variable)
      python-value-or-variable
      (let (value previous-value previous-name)
        (do-subseq-until (name python-value-or-variable #\. :test #'char=)
          (setq previous-value value)
          (cond ((string= name python-value-or-variable)
                 (setf (%pyvalue name) new-value))
                (value
                 (%pyslot-value value name))
                (t
                 (%pyvalue name)))
          (setq previous-name name))
        (when previous-value
          (setf (%pyslot-value previous-value previous-name) new-value)))))

(defun pyvalue (python-value-or-variable)
  (declare (type (or python-object string) python-value-or-variable))
  (if *in-with-remote-objects-p*
      (pyvalue* python-value-or-variable)
      (with-pygc (lispify (pyvalue* python-value-or-variable)))))

(defun (setf pyvalue) (new-value python-value-or-variable)
  (declare (type string python-value-or-variable))
  (with-pygc
    (setf (pyvalue* python-value-or-variable)
          ;; UNTRACK because we do not want to lose reference to this object!
          (pyuntrack (%pythonize new-value)))
    new-value))
