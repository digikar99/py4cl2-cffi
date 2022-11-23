(in-package :py4cl2/cffi)

;; Basic Reference: https://www.linuxjournal.com/article/8497
;; Multithreading reference: https://www.linuxjournal.com/article/3641

(defvar *python-libraries-loaded-p* nil)

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
  (/= 0 (foreign-funcall "Py_IsInitialized" :int)))

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
              (foreign-funcall "PyImport_AddModule" :string name :pointer)))))

(defun (setf py-module-pointer) (module-pointer name)
  (declare (type string name))
  (setf (gethash name *py-module-pointer-table*) module-pointer))

(defvar *py-global-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the global namespace of embedded python.")
(defvar *py-builtins-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the builtins namespace of embedded python.")

(defvar *py-output-stream* nil)
(defvar *py-output-reader-thread* nil)
(defvar *py-error-output-stream* nil)
(defvar *py-error-output-reader-thread* nil)

;; TODO: Implement the with-python-output equivalent
(defun python-output-thread ()
  (when (and *py-output-reader-thread*
             (bt:thread-alive-p *py-output-reader-thread*))
    (bt:destroy-thread *py-output-reader-thread*))
  (setq *py-output-reader-thread*
        (bt:make-thread (lambda ()
                          ;; PEEK-CHAR waits for input
                          (loop :do (peek-char nil *py-output-stream* nil)
                                    (write-char (read-char *py-output-stream* nil)
                                                *standard-output*)))))
  (when (and *py-error-output-reader-thread*
             (bt:thread-alive-p *py-error-output-reader-thread*))
    (bt:destroy-thread *py-error-output-reader-thread*))
  (setq *py-error-output-reader-thread*
        (bt:make-thread (lambda ()
                          ;; PEEK-CHAR waits for input
                          (loop :do (peek-char nil *py-error-output-stream* nil)
                                    (write-char (read-char *py-error-output-stream* nil)
                                                *error-output*))))))

(defun pystart ()

  (unless (probe-file #P"/tmp/py4cl2-cffi-output")
    (uiop:run-program "mkfifo /tmp/py4cl2-cffi-output" :output t :error-output *error-output*))
  (unless (probe-file #P"/tmp/py4cl2-cffi-error-output")
    (uiop:run-program "mkfifo /tmp/py4cl2-cffi-error-output" :output t :error-output *error-output*))

  (load-python-and-libraries)
  (foreign-funcall "Py_Initialize")
  (raw-py "import sys")
  (let ((python-output-reader-open-thread
          ;; Need to do this in a separate initialization thread to deal with
          ;; blocking 'open' for named pipes
          (bt:make-thread
           (lambda ()
             (setq *py-output-stream* (open "/tmp/py4cl2-cffi-output" :direction :input))))))
    (raw-py "sys.stdout = open('/tmp/py4cl2-cffi-output', 'w')")
    (bt:join-thread python-output-reader-open-thread))
  (let ((python-error-output-reader-open-thread
          (bt:make-thread
           (lambda ()
             (setq *py-error-output-stream*
                   (open #P"/tmp/py4cl2-cffi-error-output" :direction :input))))))
    (raw-py "sys.stderr = open('/tmp/py4cl2-cffi-error-output', 'w')")
    (bt:join-thread python-error-output-reader-open-thread))
  (python-output-thread)

  (dolist (mod '("__main__" "builtins" "sys"))
    (setf (py-module-pointer mod)
          (foreign-funcall "PyImport_AddModule" :string mod :pointer)))
  (setq *py-global-dict*
        (foreign-funcall "PyModule_GetDict" :pointer (py-module-pointer "__main__") :pointer))
  (setq *py-builtins-dict*
        (foreign-funcall "PyModule_GetDict" :pointer (py-module-pointer "builtins") :pointer))
  t)

(defun python-may-be-error ()
  (python-start-if-not-alive)
  (let ((may-be-error (foreign-funcall "PyErr_Occurred" :pointer)))
    (unless (null-pointer-p may-be-error)
      (foreign-funcall "PyErr_PrintEx")
      (foreign-funcall "PyErr_Clear")
      (error "A python error occured"))))

(defun raw-py (code-string)
  (python-start-if-not-alive)
  (unless (zerop (foreign-funcall "PyRun_SimpleString" :string code-string :int))
    (python-may-be-error)))

(defun pystop ()
  (setq *py-module-pointer-table* (make-hash-table :test #'equal))
  (setq *py-global-dict* nil)
  (setq *py-builtins-dict* nil)
  (when (python-alive-p)
    (raw-py "close(sys.stdout)")
    (raw-py "close(sys.stderr)")
    ;; It is okay to call Py_FinalizeEx even when python is not initialized
    (let ((value (foreign-funcall "Py_FinalizeEx" :int)))
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
    (foreign-funcall "PyDict_GetItemString"
                     :pointer *py-builtins-dict*
                     :pointer name
                     :pointer)))

(defun pyvalue* (name &optional (module (py-module-pointer "__main__")))
  "Get the pointer to the value associated with NAME in MODULE"
  (declare (type string name)
           (type foreign-pointer module))
  (assert (not (null-pointer-p module)))
  (python-start-if-not-alive)
  (let ((module-dict (foreign-funcall "PyModule_GetDict"
                                      :pointer module
                                      :pointer)))
    (with-foreign-string (cname name)
      (foreign-funcall "PyDict_GetItemString"
                       :pointer module-dict
                       :pointer cname
                       :pointer))))

(defun (setf pyvalue*) (value name &optional (module (py-module-pointer "__main__")))
  "Set the value associated with NAME in MODULE to the value pointed by VALUE pointer"
  (declare (type string name)
           (type foreign-pointer module value))
  (assert (not (null-pointer-p module)))
  (python-start-if-not-alive)
  (let ((module-dict (foreign-funcall "PyModule_GetDict"
                                      :pointer module
                                      :pointer)))
    (unless (zerop (foreign-funcall "PyDict_SetItemString"
                                    :pointer module-dict
                                    :string name
                                    :pointer value
                                    :int))
      (python-may-be-error))
    t))



(defun pyvalue (name &optional (module (py-module-pointer "__main__")))
  "Get the lispified value associated with NAME in MODULE"
  (declare (type string name)
           (type (or string foreign-pointer) module))
  (lispify (pyvalue* name (etypecase module
                            (string (py-module-pointer module))
                            (foreign-pointer module)))))

(defun (setf pyvalue) (value name &optional (module (py-module-pointer "__main__")))
  "Set the value associated with NAME in MODULE to the VALUE after pythonizing it"
  (declare (type string name)
           (type (or string foreign-pointer) module))
  (setf (pyvalue* name (etypecase module
                         (string (py-module-pointer module))
                         (foreign-pointer module)))
        (pythonize value)))
