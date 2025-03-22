(in-package :py4cl2-cffi)

(defvar *pymain-thread*)
(defvar *pymain-thread-fun-args-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-fun-args-semaphore"))
(defvar *pymain-thread-result-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-result-semaphore"))

(defvar *pymain-call-stack* nil)
(defvar *pymain-value-stack* nil)

(defun funcall/single-threaded (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply fun args))
        (t
         (push (cons fun args) *pymain-call-stack*)
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pop *pymain-value-stack*))))

(defun apply/single-threaded (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply #'apply fun args))
        (t
         (push (apply #'list* fun args) *pymain-call-stack*)
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pop *pymain-value-stack*))))



(defparameter *single-threaded-blacklist*
  '(pystart mkfifo raw-py define-lispifier with-lispifiers with-pythonizers
    defpyfun defpymodule)
  "List of function and macro names which will not be translated to their single-threaded.")

(defun call-form-from-fn-and-ll (fn-symbol lambda-list call-mode)
  (multiple-value-bind
        (required optional rest keywords)
      (parse-ordinary-lambda-list lambda-list)
    (multiple-value-bind (apply funcall)
        (ecase call-mode
          (:single-threaded (values 'apply/single-threaded 'funcall/single-threaded))
          (:multi-threaded (values 'apply 'funcall)))
      (cond (rest
             `(,apply
               (cl:function ,fn-symbol)
               ,@required
               ,@(mapcar #'first optional)
               ,rest))
            (keywords
             `(,funcall
               (cl:function ,fn-symbol)
               ,@required
               ,@(mapcar #'first optional)
               ,@(loop :for ((key name) init suppliedp) :in keywords
                       :nconcing (list (make-keyword key) name))))
            (t
             `(,funcall
               (cl:function ,fn-symbol)
               ,@required
               ,@(mapcar #'first optional)))))))

(defmacro define-flexible-function (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (flet ((,name ,lambda-list
              ,@body))
       (if (eq *pymain-thread* (bt:current-thread))
           ,(call-form-from-fn-and-ll name lambda-list :multi-threaded)
           ,(call-form-from-fn-and-ll name lambda-list :single-threaded)))))

(defmacro define-flexible-macro (name lambda-list &body body)
  `(defmacro ,name ,lambda-list
     (flet ((,name ,lambda-list
              ,@body))
       (if (eq *pymain-thread* (bt:current-thread))
           ,(call-form-from-fn-and-ll name lambda-list :multi-threaded)
           ,(call-form-from-fn-and-ll name lambda-list :single-threaded)))))
