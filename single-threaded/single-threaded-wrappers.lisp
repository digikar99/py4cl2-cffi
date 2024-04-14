(in-package :py4cl2-cffi/single-threaded)

(defun funcall/single-threaded (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply fun args))
        (t
         (setf (pymain-call) (cons fun args))
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pymain-result))))

(defun apply/single-threaded (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply #'apply fun args))
        (t
         (setf (pymain-call) (apply #'list* fun args))
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pymain-result))))

(eval-when (:compile-toplevel)
  (defparameter *single-threaded-blacklist*
    '(pystart mkfifo raw-py define-lispifier with-lispifiers with-pythonizers
      defpyfun defpymodule)
    "List of function and macro names which will not be translated to their single-threaded.")
  (defun call-form-from-fn-and-ll (fn lambda-list)
    (multiple-value-bind
          (required optional rest keywords)
        (parse-ordinary-lambda-list lambda-list)
      (cond (rest
             `(apply/single-threaded
               ,fn
               ,@required
               ,@(mapcar #'first optional)
               ,rest))
            (keywords
             `(funcall/single-threaded
               ,fn
               ,@required
               ,@(mapcar #'first optional)
               ,@(loop :for ((key name) init suppliedp) :in keywords
                       :nconcing (list (make-keyword key) name))))
            (t
             `(funcall/single-threaded
               ,fn
               ,@required
               ,@(mapcar #'first optional)))))))

(macrolet
    ((def (single-threaded-sym)
       (let* ((multi-threaded-sym
                (find-symbol (symbol-name single-threaded-sym)
                             :py4cl2-cffi))
              (args (gensym "ARGS"))
              (macrop (macro-function multi-threaded-sym)))
         (when (and (not (member single-threaded-sym
                                 *single-threaded-blacklist*))
                    (fboundp multi-threaded-sym)
                    (not (eq single-threaded-sym multi-threaded-sym)))
           ;; FIXME: Handle keyword args
           `(,(let ((lambda-list
                      (swank/backend:arglist multi-threaded-sym)))
                `(,(if macrop 'defmacro 'defun)
                  ,single-threaded-sym ,lambda-list
                  ,(call-form-from-fn-and-ll
                    `(lambda (&rest ,args)
                       (float-features:with-float-traps-masked t
                         ,(if macrop
                              `(funcall (macro-function ',multi-threaded-sym)
                                        (list* ',multi-threaded-sym ,args) nil)
                              `(apply #',multi-threaded-sym ,args))))
                    lambda-list)))
             ,(when (fboundp `(setf ,multi-threaded-sym))
                (let ((lambda-list
                        (swank/backend:arglist `(setf ,multi-threaded-sym))))
                  `(,(if macrop 'defmacro 'defun)
                    (setf ,single-threaded-sym) ,lambda-list
                    ,(call-form-from-fn-and-ll
                      `(lambda (&rest ,args)
                         (float-features:with-float-traps-masked t
                           ,(if macrop
                                `(apply (macro-function '(setf ,multi-threaded-sym))
                                        ,args)
                                `(apply #',multi-threaded-sym ,args))
                           (apply #'(setf ,multi-threaded-sym) ,args)))
                      lambda-list))))))))

     (def-all (&environment env)
       (let ((all nil))
         (do-external-symbols (s :py4cl2-cffi/single-threaded)
           (nconcf all (macroexpand `(def ,s) env)))
         ;; Additional whitelisted names.
         (dolist (s `(py-module-pointer
                      load-python-and-libraries
                      mkfifo
                      pycall*
                      pyeval-save-thread
                      pygil-held-p
                      pyvalue*))
           (nconcf all (macroexpand `(def ,s) env)))
         `(progn
            ,@all))))

  (def-all))
