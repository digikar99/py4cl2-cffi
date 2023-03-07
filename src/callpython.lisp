(in-package :py4cl2-cffi)

(defmacro with-remote-objects (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets."
  `(thread-global-let ((*in-with-remote-objects-p* t))
     ,@body))

(defmacro with-remote-objects* (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets. Unlike
with-remote-objects, evaluates the last result and returns not just a handle."
  `(with-pygc
     (thread-global-let ((*in-with-remote-objects-p* nil))
       (lispify (with-remote-objects ,@body)))))

(defun pythonize-args (lisp-args)
  (loop :for arg :in lisp-args
        :collect (if (python-keyword-p arg)
                     arg
                     (%pythonize arg))))

(defun args-and-kwargs (lisp-args)
  (let ((first-keyword-position
          (or (position-if #'python-keyword-p lisp-args)
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
      lispified-return-value)))

;; %PYCALL and %PYCALL* take a pointer to callable as the argument
;; but the rest of the arguments can be lisp objects and need not be pointers.

(defun %pycall* (python-callable-pointer &rest args)
  (declare (type foreign-pointer python-callable-pointer)
           (optimize debug))
  (labels ((pin-and-call (&rest rem-args)
             (cond ((null rem-args)
                    (let ((pythonized-args (pythonize-args args)))
                      (multiple-value-bind (pos-args kwargs)
                          (args-and-kwargs pythonized-args)
                        ;; PyObject_Call returns a new reference
                        (let* ((return-value
                                 (pyforeign-funcall "PyObject_Call"
                                                    :pointer python-callable-pointer
                                                    :pointer pos-args
                                                    :pointer kwargs
                                                    :pointer)))
                          return-value))))
                   ((and (arrayp (car rem-args))
                         (not (eq t (array-element-type (car rem-args)))))
                    (cffi:with-pointer-to-vector-data
                        (ptr (array-storage (car rem-args)))
                      (declare (ignore ptr))
                      (apply #'pin-and-call (rest rem-args))))
                   (t
                    (apply #'pin-and-call (rest rem-args))))))
    (apply #'pin-and-call args)))

(defun %pycall (python-callable-pointer &rest args)
  (declare (type foreign-pointer python-callable-pointer))
  (if *in-with-remote-objects-p*
      (apply #'%pycall* python-callable-pointer args)
      (with-pygc
        ;; We can't just rely on %PYCALL* because we also need to deal with
        ;; PYTHONIZED-ARGS while lispifying the values
        (let ((pythonized-args (pythonize-args args)))
          (multiple-value-bind (pos-args kwargs)
              (args-and-kwargs pythonized-args)
            ;; PyObject_Call returns a new reference
            (let* ((return-value (pyforeign-funcall "PyObject_Call"
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
              (%pycall-return-value return-value)))))))

(labels ((pythonizep (value)
           "Determines if VALUE should be pythonized."
           (or (not (stringp value)) ; do not pythonize if
               (realp (ignore-errors (parse-number:parse-number value)))))
         (pythonize-if-needed (value)
           (if (pythonizep value)
               (py-repr value)
               value)))
  (defun pyeval (&rest args)
    (python-start-if-not-alive)
    (apply #'raw-pyeval (mapcar #'pythonize-if-needed args)))
  (defun pyexec (&rest args)
    (python-start-if-not-alive)
    (apply #'raw-pyexec (mapcar #'pythonize-if-needed args)))
  (defun (setf pyeval) (value &rest args)
    (python-start-if-not-alive)
    (apply #'pyexec (nconc args (list "=" value)))
    value))

(defun python-name-p (name)
  (declare (optimize speed))
  (and (stringp name)
       (every (lambda (char)
                (or (alphanumericp char)
                    (char= char #\.)))
              name)))
(deftype python-name ()
  `(and string (satisfies python-name-p)))

(defun pycall* (python-callable &rest args)
  "If PYTHON-CALLABLE is a string or symbol, it is treated as the name of a
python callable, which is then retrieved using PYVALUE*"
  (declare (optimize debug))
  (python-start-if-not-alive)
  (let ((pyfun (typecase python-callable
                 (python-object
                  (python-object-pointer python-callable))
                 (python-name
                  (pyvalue* python-callable))
                 (string
                  (with-remote-objects (raw-pyeval python-callable)))
                 (symbol
                  (pyvalue* (pythonize-symbol python-callable)))
                 (t
                  (pythonize python-callable)))))
    (if (null-pointer-p pyfun)
        (error "Python function ~A is not defined" python-callable)
        (apply #'%pycall* pyfun args))))

(defun pycall (python-callable &rest args)
  "If PYTHON-CALLABLE is a string or symbol, it is treated as the name of a
python callable, which is then retrieved using PYVALUE*"
  (declare (optimize debug))
  (python-start-if-not-alive)
  (if *in-with-remote-objects-p*
      (apply #'pycall* python-callable args)
      (with-pygc (%pycall-return-value (apply #'pycall* python-callable args)))))

(defun pyslot-value* (object slot-name)
  (let* ((object-pointer (%pythonize object))
         (return-value   (%pyslot-value object-pointer slot-name)))
    return-value))

(defun pyslot-value (object slot-name)
  (declare (type (or symbol string) slot-name)
           (optimize debug))
  (python-start-if-not-alive)
  (if *in-with-remote-objects-p*
      (pyslot-value* object slot-name)
      (with-pygc (%pycall-return-value (pyslot-value* object slot-name)))))

(defun (setf pyslot-value) (new-value object slot-name)
  (python-start-if-not-alive)
  (with-pygc
    (let* ((object-pointer (%pythonize object))
           (new-value      (%pythonize new-value)))
      (setf (%pyslot-value object-pointer slot-name) new-value))
    new-value))

(defun pymethod (object method-name &rest args)
  (declare (optimize debug))
  (python-start-if-not-alive)
  (with-pygc
    (let* ((object-pointer (%pythonize object))
           (method         (%pyslot-value object-pointer method-name)))
      (apply #'%pycall method args))))

(defun pyhelp (string-or-python-callable)
  (pycall "help" string-or-python-callable)
  (pycall (pyvalue "sys.stdout.flush"))
  nil)

(flet ((may-be-slice (index)
         (if (and (listp index)
                  (typep (first index) 'string-designator)
                  (string-equal "slice" (first index)))
             (apply #'pycall* "slice" (rest index))
             index)))

  (defun pyref (object &rest indices)
    (pymethod object "__getitem__"
              (if (= 1 (length indices))
                  (may-be-slice (first indices))
                  (pycall* "tuple"
                           (mapcar #'may-be-slice indices)))))

  (defun (setf pyref) (new-value object &rest indices)
    (pymethod object "__setitem__"
              (if (= 1 (length indices))
                  (may-be-slice (first indices))
                  (pycall* "tuple"
                           (loop :for index :in indices
                                 :collect (may-be-slice indices))))
              new-value)))

(defun %chain* (link &optional initial-value)
  (if initial-value
      (optima:match link
        ((list* 'aref args)
         (apply #'pyref initial-value args))
        ((optima:guard _ (and (consp link)
                              (consp (first link))))
         (apply #'pymethod initial-value
                (mapcar #'chain* link)))
        ((optima:guard (list* _ chain)
                       (and (typep (first link) 'string-designator)
                            (member (first link) '("@" "CHAIN") :test #'string=)))
         (pyslot-value initial-value (apply #'chain* chain)))
        ((list* name args)
         (apply #'pymethod initial-value name args))
        (_
         (pyslot-value initial-value link)))
      (optima:match link
        ((list* 'aref object args)
         (apply #'pyref object args))
        ((optima:guard _ (and (consp link)
                              (consp (first link))))
         (apply #'pycall
                (mapcar #'chain* link)))
        ((optima:guard (list* _ chain)
                       (and (typep (first link) 'string-designator)
                            (member (first link) '("@" "CHAIN") :test #'string=)))
         (apply #'chain* chain))
        ((list* name args)
         (apply #'pycall name args))
        (_
         (%pythonize link)))))

(defun chain* (&rest chain)
  (with-pygc
    (loop :for link :in chain
          :with value := nil
          :do (setq value (%chain* link value))
          :finally (return value))))

(defun (setf chain*) (new-value &rest chain)
  (let ((last  (lastcar chain))
        (chain (butlast chain)))
    (with-pygc
      (loop :with value := nil
            :for link :in chain
            :do (setq value (%chain* link value))
            :finally (return (setf (pyslot-value value last) new-value))))))

(defmacro chain (&rest chain)
  `(chain* ,@(loop :for link :in chain
                   :collect `(quote ,link))))

(defun pyversion-info ()
  "Return a list, using the result of python's sys.version_info."
  (pycall "tuple" (pyvalue "sys.version_info")))

(defun pygenerator (function stop-value)
  (with-pygc
    (pycall "_py4cl_generator" function stop-value)))
