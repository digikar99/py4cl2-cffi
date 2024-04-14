(in-package :py4cl2-cffi/single-threaded)

(defun get-arg-list/single-threaded (fullname lisp-package)
  "Returns a list of two lists: PARAMETER-LIST and PASS-LIST"
  (declare (optimize debug))
  (destructuring-bind (parameter-list (first second))
      (get-arg-list fullname lisp-package)
    (list parameter-list
          (list first
                (optima:ematch second
                  ((list* 'py4cl2-cffi:pycall args)
                   `(pycall ,@args))
                  ((list* 'apply (list 'function 'py4cl2-cffi:pycall) args)
                   `(apply #'pycall ,@args)))))))

(defmacro defpyfun (fun-name
                    &optional pymodule-name
                    &key
                      (as fun-name)
                      (cache t)
                      (lisp-fun-name (lispify-name as))
                      (lisp-package *package*)
                      (safety t))
  (let ((*arg-list-fun* 'get-arg-list/single-threaded))
    (macroexpand `(py4cl2-cffi:defpyfun ,fun-name ,pymodule-name
                    :as ,as :cache ,cache :lisp-fun-name ,lisp-fun-name
                    :lisp-package ,lisp-package :safety ,safety))))

(defmacro defpymodule (pymodule-name
                       &optional (import-submodules nil)
                       &rest args
                       &key (cache t)
                         (continue-ignoring-errors t)
                         (lisp-package (lispify-name pymodule-name) lisp-package-supplied-p)
                         (reload t)
                         (recompile-on-change nil)
                         (safety t)
                         (silent *defpymodule-silent-p*))
  (declare (ignore cache continue-ignoring-errors
                   lisp-package lisp-package-supplied-p
                   reload
                   recompile-on-change
                   safety
                   silent))
  (let ((*arg-list-fun* 'get-arg-list/single-threaded))
    (macroexpand `(py4cl2-cffi:defpymodule
                      ,pymodule-name ,import-submodules ,@args))))
