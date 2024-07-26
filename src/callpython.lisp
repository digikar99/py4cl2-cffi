(in-package :py4cl2-cffi)

(defmacro with-remote-objects (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only pointers are returned to lisp.
This is useful if performing operations on large datasets."
  `(thread-global-let ((*pyobject-translation-mode* :wrapper))
     ,@body))

(defmacro with-remote-objects* (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets. Unlike
with-remote-objects, evaluates the last result and returns not just a handle."
  `(with-pygc
     (thread-global-let ((*pyobject-translation-mode* :lisp))
       (lispify (pyobject-wrapper-pointer (with-remote-objects ,@body))))))

;; FIXME: This is probably thread unsafe!
(defconstant +python-arguments-limit+ 1000000)
(let ((python-array-pointers
        (make-array +python-arguments-limit+ :element-type '(unsigned-byte 64)))
      (lisp-arrays
        (make-array +python-arguments-limit+ :element-type t)))

  (defun pythonize-args (lisp-args)
    (declare (optimize speed)
             (dynamic-extent lisp-args)
             (inline pytuple-new pytuple-setitem pydict-setitem))
    (let* ((len (list-length lisp-args))
           (first-keyword-position
             (loop :for arg :in lisp-args
                   :for pos :of-type (unsigned-byte 32) :from 0
                   :with first-keyword-position := nil
                   :do (cond ((and (null first-keyword-position)
                                   (python-keyword-p arg))
                              (setq first-keyword-position pos))
                             ((arrayp arg)
                              (setf (aref lisp-arrays pos) arg)))
                   :finally (return first-keyword-position)))
           (idx 0))
      (declare (type (unsigned-byte 32) idx))
      (flet ((pythonize-list (list)
               (declare (dynamic-extent list))
               (let* ((tuple (pytuple-new (or first-keyword-position len))))
                 (assert (not (null-pointer-p tuple)))
                 (loop :for elt :in list
                       :for pyelt := (pythonize elt)
                       :for pos :from 0
                       :do (assert (zerop (pytuple-setitem tuple pos pyelt)))
                           (pyuntrack pyelt)
                           (when (arrayp elt)
                             (setf (aref python-array-pointers pos)
                                   (pointer-address pyelt)))
                       :finally (setf idx pos))
                 tuple))
             (pythonize-plist (plist)
               (let ((dict (pydict-new)))
                 (doplist (key val plist)
                          (let ((pyval (pythonize val)))
                            (setf (aref python-array-pointers (1+ idx))
                                  (pointer-address pyval))
                            (assert (zerop (pydict-setitem dict
                                                           (pythonize key)
                                                           pyval)))
                            (incf idx 2)))
                 dict)))
        (if (null first-keyword-position)
            (values len
                    (pythonize-list lisp-args)
                    nil)
            ;; The only translation of lisp keywords to python
            ;; is that of treating them like python keyword args
            (values len
                    (pythonize-list  (subseq lisp-args 0 first-keyword-position))
                    (pythonize-plist (subseq lisp-args first-keyword-position)))))))

  ;; If the RETURN-VALUE is an array amongst the inputs,
  ;; then avoid lispifying the return-value
  (defun may-be-lispify-array (len pyobject-ptr)
    (declare (type (unsigned-byte 32) len)
             (type foreign-pointer pyobject-ptr)
             (optimize speed))
    (let ((addr (pointer-address pyobject-ptr)))
      (loop :for i :below len
            :do (when (= addr (aref python-array-pointers i))
                  (return (aref lisp-arrays i))))))

  (defun clear-pythonize-array-cache (len)
    (declare (optimize speed)
             (type (unsigned-byte 32) len))
    (loop :for i :below len
          :do (setf (aref python-array-pointers i) 0)
              (setf (aref lisp-arrays i) nil))))

(declaim (inline %pycall-return-value))
(defun %pycall-return-value (return-value)
  (declare (optimize speed))
  (if (typep return-value 'foreign-pointer)
      (lispify return-value)
      return-value))

;; %PYCALL and %PYCALL* take a pointer to callable as the argument.
;; %PYCALL* and PYCALL* return a pointer to the return value
;;   without wrapping or lispifying it.
;; The arguments to all the four variants can be lisp objects
;;   and need not be pointers.

(declaim (ftype (function (foreign-pointer &rest t)
                          (values foreign-pointer &optional))
                %pycall*))
(defun %pycall* (python-callable-pointer &rest args)
  "Fastest (non compile-time) variant of PYCALL.
It takes in a foreign-pointer to a python callable and returns a foreign pointer to the return value which is a pyobject."
  (declare (type foreign-pointer python-callable-pointer)
           (optimize speed))
  ;; It is not appropriate to use PYGC here.
  ;; We expect the task of GC-ing to be performed by higher level functions.
  (labels ((pin-and-call (&rest rem-args)
             (cond ((null rem-args)
                    (multiple-value-bind (len pos-args kwargs)
                        (pythonize-args args)
                      ;; PyObject_Call returns a new reference
                      (let* ((return-value
                               (if (null kwargs)
                                   (pyobject-callobject python-callable-pointer
                                                        pos-args)
                                   (pyobject-call python-callable-pointer
                                                  pos-args kwargs))))
                        (clear-pythonize-array-cache len)
                        return-value)))
                   ((and (arrayp (car rem-args))
                         (not (eq t (array-element-type (car rem-args)))))
                    (cffi:with-pointer-to-vector-data
                        (ptr (array-storage (car rem-args)))
                      #-ccl (declare (ignore ptr))
                      (apply #'pin-and-call (rest rem-args))))
                   (t
                    (apply #'pin-and-call (rest rem-args))))))
    (apply #'pin-and-call args)))

(defun %pycall (python-callable-pointer &rest args)
  (declare (type foreign-pointer python-callable-pointer)
           (optimize speed)
           (inline pyobject-callobject pyobject-call))
  (ecase *pyobject-translation-mode*
    (:foreign-pointer (apply #'%pycall* python-callable-pointer args))
    (:wrapper
     (let ((pyobject-pointer (apply #'%pycall* python-callable-pointer args)))
       (pyuntrack pyobject-pointer)
       (make-tracked-pyobject-wrapper pyobject-pointer)))
    (:lisp
     (with-pygc
       ;; We can't just rely on %PYCALL* because we also need to deal with
       ;; PYTHONIZED-ARGS while lispifying the values
       (multiple-value-bind (len pos-args kwargs)
           (pythonize-args args)
         ;; PyObject_Call returns a new reference
         (let* ((return-value (if (null kwargs)
                                  (pyobject-callobject python-callable-pointer
                                                       pos-args)
                                  (pyobject-call python-callable-pointer
                                                 pos-args kwargs)))
                (lisp-return-value
                  (or (may-be-lispify-array len return-value)
                      (%pycall-return-value return-value))))
           (clear-pythonize-array-cache len)
           lisp-return-value))))))

(labels ((pythonizep (value)
           "Determines if VALUE should be pythonized."
           (or (not (stringp value))    ; do not pythonize if
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

(declaim (inline python-name-p))
(defun python-name-p (name)
  (declare (optimize speed))
  (and (stringp name)
       (every (lambda (char)
                (or (alphanumericp char)
                    (member char '(#\. #\_) :test #'char=)))
              name)))
(deftype python-name ()
  `(and string (satisfies python-name-p)))

(defun pycall* (python-callable &rest args)
  "If PYTHON-CALLABLE is a string or symbol, it is treated as the name of a
python callable, which is then retrieved using PYVALUE*"
  (declare (optimize speed))
  (python-start-if-not-alive)
  (let ((pyfun (typecase python-callable
                 (python-name
                  (pyvalue* python-callable))
                 (string
                  (raw-py #\e python-callable))
                 (symbol
                  (pyvalue* (pythonize-symbol python-callable)))
                 (pyobject-wrapper
                  (pyobject-wrapper-pointer python-callable))
                 (t
                  (pythonize python-callable)))))
    (if (null-pointer-p pyfun)
        (error "Python function ~A is not defined" python-callable)
        (apply #'%pycall* pyfun args))))

(declaim (inline pyobject-pointer-translate))
(defun pyobject-pointer-translate (pyobject-pointer)
  (declare (optimize speed)
           (type foreign-pointer pyobject-pointer)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (ecase *pyobject-translation-mode*
    (:foreign-pointer pyobject-pointer)
    (:wrapper
     (pyuntrack pyobject-pointer)
     (make-tracked-pyobject-wrapper pyobject-pointer))
    (:lisp
     (with-pygc (%pycall-return-value pyobject-pointer)))))

(defun pycall (python-callable &rest args)
  "If PYTHON-CALLABLE is a string or symbol, it is treated as the name of a
python callable, which is then retrieved using PYVALUE*"
  (declare (optimize speed))
  (python-start-if-not-alive)
  (pyobject-pointer-translate (apply #'pycall* python-callable args)))

(defun pyslot-value* (object slot-name)
  (let* ((object-pointer (%pythonize object))
         (return-value   (%pyslot-value object-pointer slot-name)))
    return-value))

(defun pyslot-value (object slot-name)
  (declare (type (or symbol string) slot-name)
           (optimize debug))
  (python-start-if-not-alive)
  (pyobject-pointer-translate (pyslot-value* object slot-name)))

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
                                 :collect (may-be-slice index))))
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
