(in-package :py4cl2/cffi)

;; (defcstruct pyobject )

;; Basic Reference: https://www.linuxjournal.com/article/8497
;; Multithreading reference: https://www.linuxjournal.com/article/3641

;; TODO: Handling errors

(defun load-libraries ()
  (let ((libraries '("python3.8" "crypt" "m" "pthread" "dl" "rt")))
    (dolist (lib ;; (uiop:directory-files
             ;;  #P"/media/common-storage/miniconda3/lib/python3.8/config-3.8-x86_64-linux-gnu/")
             ;; TODO: Avoid hardcoding paths
             (cons #P"/home/shubhamkar/quicklisp/local-projects/py4cl2/cffi/libpychecks.so"
                   (loop :for lib :in libraries
                         :nconcing
                         (uiop:directory-files
                          #P"/media/common-storage/miniconda3/lib/" (format nil "lib~A.so*" lib)))))
      (load-foreign-library lib))))

(defun raw-py (code-string)
  (with-foreign-string (str code-string)
    (foreign-funcall "PyRun_SimpleString" :pointer str :int)))

(defvar *py-main-module* nil
  "Pointer to the __main__ module of the embedded python.")
(defvar *py-builtins-module* nil
  "Pointer to the builtin module of the embedded python.")
(defvar *py-global-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the global namespace of embedded python.")
(defvar *py-builtins-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the builtins namespace of embedded python.")

(defun pystart ()
  (load-libraries)
  (foreign-funcall "Py_Initialize")
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
