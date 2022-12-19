(in-package :py4cl2/cffi)

(defun pythonize-args (lisp-args)
  (loop :for arg :in lisp-args
        :collect (if (keywordp arg)
                     arg
                     (pythonize arg))))

(defun args-and-kwargs (lisp-args)
  (let ((first-keyword-position
          (or (position-if #'keywordp lisp-args)
              (length lisp-args))))
    ;; The only translation of lisp keywords to python
    ;; is that of treating them like python keyword args
    (values (pythonize-list (subseq lisp-args 0 first-keyword-position))
            (pythonize-plist (subseq lisp-args first-keyword-position)))))

(defun %pycall-return-value (return-value)
  (with-python-exceptions
    ;; FIXME: Why did we write it this way?
    (let ((lispified-return-value (if (typep return-value 'foreign-pointer)
                                      (lispify return-value)
                                      return-value)))
      (when (typep lispified-return-value 'foreign-pointer)
        (pytrack return-value))
      lispified-return-value)))

(defun %pycall (python-callable-pointer &rest args)
  (declare (type foreign-pointer python-callable-pointer)
           (optimize debug))
  (with-pygc
    (let ((pythonized-args (pythonize-args args)))
      (multiple-value-bind (pos-args kwargs)
          (args-and-kwargs pythonized-args)
        (let* ((return-value (foreign-funcall "PyObject_Call"
                                              :pointer python-callable-pointer
                                              :pointer pos-args
                                              :pointer kwargs
                                              :pointer)))
          ;; If the RETURN-VALUE is an array amongst the inputs,
          ;; then avoid lispifying the return-value
          (mapc (lambda (pyarg arg)
                  (when (and (arrayp arg)
                             (pointer-eq pyarg return-value))
                    (setq return-value arg)))
                pythonized-args args)
          (%pycall-return-value return-value))))))

(defun pyeval (&rest args)
  ;; FIXME: This isn't how it should be? We need strings
  (apply #'raw-pyeval (mapcar #'pythonize args)))

(defun pycall (python-callable &rest args)
  "If PYTHON-CALLABLE is a string, it is treated as the name of a
python callable, which is then retrieved using PYVALUE*"
  (declare (optimize debug))
  (python-start-if-not-alive)
  (with-pygc
    (let ((pyfun (typecase python-callable
                   (python-object
                    (python-object-pointer python-callable))
                   (string
                    (pyvalue* python-callable))
                   (t
                    (pythonize python-callable)))))
      (if (null-pointer-p pyfun)
          (error "Python function ~A is not defined" python-callable)
          (apply #'%pycall pyfun args)))))

;;; TODO: Define (SETF PYSLOT-VALUE)
(defun pyslot-value (object slot-name)
  (declare (type string slot-name)
           (optimize debug))
  (python-start-if-not-alive)
  (with-pygc
    (let* ((object-pointer (pythonize object))
           (return-value   (%pyslot-value object-pointer slot-name)))
      (%pycall-return-value return-value))))

(defun pymethod (object method-name &rest args)
  (declare (type string method-name)
           (optimize debug))
  (python-start-if-not-alive)
  (let* ((object-pointer (pythonize object))
         (method         (%pyslot-value object-pointer method-name)))
    (or (apply #'%pycall method args)
        (lispify object-pointer))))

(defun pyhelp (string-or-python-callable)
  (pycall "help" string-or-python-callable)
  (pymethod (pyvalue "sys.stdout") "flush")
  nil)

(defun %chain* (link)
  (etypecase link
    (list (apply #'pycall (first link) (rest link)))
    (atom (pythonize link))))

;; FIXME: How exactly do we want to handle strings
(defun chain* (&rest chain)
  (loop :for link :in (rest chain)
        :with value := (%chain* (first chain))
        :do (setq value (etypecase link
                          (list (apply #'pymethod value link))
                          (atom (pyslot-value value link))))
        :finally (return value)))

(defun @ (&rest chain)
  (apply #'chain chain))

(defun pyversion-info ()
  "Return a list, using the result of python's sys.version_info."
  (pycall "tuple" (pyvalue "sys.version_info")))
