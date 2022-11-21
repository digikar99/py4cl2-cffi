(in-package :py4cl2/cffi)

(defun args-and-kwargs (lisp-args)
  (let ((first-keyword-position
          (or (position-if #'keywordp lisp-args)
              (length lisp-args))))
    ;; The only translation of lisp keywords to python
    ;; is that of treating them like python keyword args
    (values (pythonize-list (subseq lisp-args 0 first-keyword-position))
            (pythonize-plist (subseq lisp-args first-keyword-position)))))

(defun pycall (name &rest args)
  (with-foreign-string (cname name)
    (let* ((pyfun (foreign-funcall "PyDict_GetItemString"
                                   :pointer *py-global-dict*
                                   :pointer cname
                                   :pointer))
           (pyfun (cond ((null-pointer-p pyfun)
                         (foreign-funcall "PyDict_GetItemString"
                                          :pointer *py-builtins-dict*
                                          :pointer cname
                                          :pointer))
                        (t
                         pyfun))))
      (if (null-pointer-p pyfun)
          (error "Python function ~A is not defined" name)
          (multiple-value-bind (args kwargs)
              (args-and-kwargs args)
            (let* ((return-value (foreign-funcall "PyObject_Call"
                                                  :pointer pyfun
                                                  :pointer args
                                                  :pointer kwargs
                                                  :pointer))
                   (may-be-exception-type (foreign-funcall "PyErr_Occurred" :pointer)))
              (if (null-pointer-p may-be-exception-type)
                  ;; return-value
                  (lispify return-value)
                  (python-error))))))))
