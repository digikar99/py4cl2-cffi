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
  (declare (type string name))
  (python-start-if-not-alive)
  (multiple-value-bind (module name)
      (let ((dot-pos (position #\. name)))
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
          (multiple-value-bind (args kwargs)
              (args-and-kwargs args)
            (unwind-protect
                 (let* ((return-value (pytrack
                                       (foreign-funcall "PyObject_Call"
                                                        :pointer pyfun
                                                        :pointer args
                                                        :pointer kwargs
                                                        :pointer)))
                        (may-be-exception-type (foreign-funcall "PyErr_Occurred" :pointer)))
                   (if (null-pointer-p may-be-exception-type)
                       ;; return-value
                       (lispify return-value)
                       (python-may-be-error)))
              (pygc)))))))
