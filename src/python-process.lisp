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
             (cons #P"/home/shubhamkar/quicklisp/local-projects/py4cl2/cffi/libpychecks.so"
                   (loop :for lib :in libraries
                         :nconcing
                         (uiop:directory-files
                          #P"/media/common-storage/miniconda3/lib/" (format nil "lib~A.so*" lib)))))
      (load-foreign-library lib))))

(defun python-error ()
  (let ((may-be-error (foreign-funcall "PyErr_Occurred" :pointer)))
    (unless (null-pointer-p may-be-error)
      (foreign-funcall "PyErr_PrintEx")
      (foreign-funcall "PyErr_Clear")
      (error "A python error occured"))))

(defun raw-py (code-string)
  (with-foreign-string (str code-string)
    (unless (zerop (foreign-funcall "PyRun_SimpleString" :pointer str :int))
      (python-error))))

(defvar *py-main-module* nil
  "Pointer to the __main__ module of the embedded python.")
(defvar *py-builtins-module* nil
  "Pointer to the builtin module of the embedded python.")
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

  (setq *py-main-module*
        (with-foreign-string (main "__main__")
          (foreign-funcall "PyImport_AddModule" :pointer main :pointer)))
  (setq *py-builtins-module*
        (with-foreign-string (mod "builtins")
          (foreign-funcall "PyImport_AddModule" :pointer mod :pointer)))
  (setq *py-global-dict*
        (foreign-funcall "PyModule_GetDict" :pointer *py-main-module* :pointer))
  (setq *py-builtins-dict*
        (foreign-funcall "PyModule_GetDict" :pointer *py-builtins-module* :pointer))
  t)

(defun pystop ()
  (setq *py-main-module* nil)
  (setq *py-builtins-module* nil)
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

(defun pyvalue* (name &optional (module *py-main-module*))
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

(defun pyvalue (name &optional (module *py-main-module*))
  "Get the lispified value associated with NAME in MODULE"
  (declare (type string name)
           (type foreign-pointer module))
  (lispify (pyvalue* name module)))

(pystart) ;; FIXME: Better way to do things
