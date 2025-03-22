(in-package :py4cl2-cffi)

(defvar *pymain-thread*)
(defvar *pymain-thread-fun-args-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-fun-args-semaphore"))
(defvar *pymain-thread-result-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-result-semaphore"))

(defstruct (pymain-call-and-result (:conc-name %py-))
  (fun-and-args)
  (result))
(let ((o (make-pymain-call-and-result)))
  (defun pymain-call ()
    (declare (optimize speed))
    (pop (%py-fun-and-args o)))
  (defun (setf pymain-call) (fun-and-args)
    (declare (optimize speed))
    (push fun-and-args (%py-fun-and-args o)))
  (defun pymain-result ()
    (declare (optimize speed))
    (pop (%py-result o)))
  (defun (setf pymain-result) (result)
    (declare (optimize speed))
    (push result (%py-result o))))

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
