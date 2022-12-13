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
  (let* ((may-be-exception-type (foreign-funcall "PyErr_Occurred" :pointer)))
    (if (null-pointer-p may-be-exception-type)
        ;; return-value
        (let ((lispified-return-value (if (typep return-value 'foreign-pointer)
                                          (lispify return-value)
                                          return-value)))
          (when (typep lispified-return-value 'foreign-pointer)
            (pytrack return-value))
          lispified-return-value)
        (python-may-be-error))))

(defun %pycall (python-callable-pointer &rest args)
  (declare (type foreign-pointer python-callable-pointer))
  (let ((pythonized-args (pythonize-args args)))
    (multiple-value-bind (pos-args kwargs)
        (args-and-kwargs pythonized-args)
      (unwind-protect
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
             (%pycall-return-value return-value))
        ;; FIXME: When to call CLEAR-LISP-OBJECTS
        (pygc)))))

(defun pycall (name &rest args)
  (declare (type string name)
           (optimize debug))
  (python-start-if-not-alive)
  (multiple-value-bind (module name)
      (let ((dot-pos (position #\. name :from-end t)))
        (if dot-pos
            (values (subseq name 0 dot-pos)
                    (subseq name (1+ dot-pos)))
            (values nil name)))
    (let* ((pyfun (cond (module
                         (foreign-funcall "PyDict_GetItemString"
                                          :pointer (py-module-dict module)
                                          :string name
                                          :pointer))
                        (t
                         (let ((global-fun
                                 (foreign-funcall "PyDict_GetItemString"
                                                  :pointer *py-global-dict*
                                                  :string name
                                                  :pointer))
                               (builtin-fun
                                 (foreign-funcall "PyDict_GetItemString"
                                                  :pointer *py-builtins-dict*
                                                  :string name
                                                  :pointer)))
                           (if (null-pointer-p global-fun)
                               builtin-fun
                               global-fun))))))
      (if (null-pointer-p pyfun)
          (error "Python function ~A is not defined" name)
          (apply #'%pycall pyfun args)))))

;;; TODO: Define (SETF PYSLOT-VALUE)
(defun pyslot-value (object slot-name)
  (declare (type string slot-name)
           (optimize debug))
  (python-start-if-not-alive)
  (unwind-protect
       (let* ((object (pythonize object))
              (return-value (foreign-funcall "PyObject_GetAttrString"
                                             :pointer object
                                             :string slot-name
                                             :pointer)))
         (%pycall-return-value return-value))
    (pygc)))

(defun pymethod (object method-name &rest args)
  (declare (type string method-name)
           (optimize debug))
  (python-start-if-not-alive)
  (let* ((pyobject (pythonize object))
         (method   (foreign-funcall "PyObject_GetAttrString"
                                    :pointer pyobject
                                    :string method-name
                                    :pointer)))
    (or (apply #'%pycall method args)
        (lispify pyobject))))

(defun pyhelp (string-or-python-callable)
  (pycall "help" string-or-python-callable)
  (pymethod (pyvalue "sys.stdout") "flush")
  nil)

