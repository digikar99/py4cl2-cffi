(in-package :py4cl2/cffi)

;; (defcstruct pyobject )

;; Basic Reference: https://www.linuxjournal.com/article/8497
;; Multithreading reference: https://www.linuxjournal.com/article/3641

;; TODO: Handling errors

(defun load-libraries ()
  (let ((libraries '("python3.8" "crypt" "m" "pthread" "dl" "rt")))
    (dolist (lib ;; (uiop:directory-files
             ;;  #P"/media/common-storage/miniconda3/lib/python3.8/config-3.8-x86_64-linux-gnu/")
             ;; TODO: Avoid hardcoding paths and test it on CI
             (nconc
              (loop :for lib :in libraries
                    :nconcing
                    (uiop:directory-files
                     #P"/media/common-storage/miniconda3/lib/" (format nil "lib~A.so*" lib)))
              '(#P"/home/shubhamkar/quicklisp/local-projects/py4cl2/cffi/libpychecks.so")))
      (load-foreign-library lib))))

(defun python-may-be-error ()
  (let ((may-be-error (foreign-funcall "PyErr_Occurred" :pointer)))
    (unless (null-pointer-p may-be-error)
      (foreign-funcall "PyErr_PrintEx")
      (foreign-funcall "PyErr_Clear")
      (error "A python error occured"))))

(defun raw-py (code-string)
  (with-foreign-string (str code-string)
    (unless (zerop (foreign-funcall "PyRun_SimpleString" :pointer str :int))
      (python-may-be-error))))

(defvar *py-module-pointer-table* (make-hash-table :test #'equal)
  "Key: A string indicating module name
Value: The pointer to the module in embedded python")
(declaim (type hash-table *py-module-pointer-table*))

(defun py-module-pointer (name)
  (declare (type string name))
  (or (nth-value 0 (gethash name *py-module-pointer-table*))
      (setf (py-module-pointer name)
            (foreign-funcall "PyImport_AddModule" :string name :pointer))))

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

  (load-libraries)
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

(defun pystop ()
  (setq *py-module-pointer-table* (make-hash-table :test #'equal))
  (setq *py-global-dict* nil)
  (setq *py-builtins-dict* nil)
  (raw-py "close(sys.stdout)")
  (raw-py "close(sys.stderr)")
  (foreign-funcall "Py_Finalize"))

(defun pytype (name)
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

(pystart) ;; FIXME: Better way to do things
