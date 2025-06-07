(in-package :py4cl2-cffi)

(defvar *pymain-thread*)
(defvar *pymain-thread-fun-args-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-fun-args-semaphore"))
(defvar *pymain-thread-result-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-result-semaphore"))

(defvar *pymain-call-lock* (bt:make-lock "py4cl2-cffi-main-thread-lock"))
(defvar *pymain-call-stack* nil)
(defvar *pymain-result-stack* nil)
(defvar *pymain-error* nil)

(declaim (inline ensure-no-error))
(defun ensure-no-error (value)
  (if (and (typep value 'error)
           *pymain-error*)
      (error (progn
               (setf *pymain-error* nil)
               value))
      value))

(defun funcall/dedicated-thread (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply fun args))
        (t
         (bt:with-lock-held (*pymain-call-lock*)
           (push (cons fun args) *pymain-call-stack*)
           (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
           (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
           (values-list (ensure-no-error (pop *pymain-result-stack*)))))))

(defun apply/dedicated-thread (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply #'apply fun args))
        (t
         (bt:with-lock-held (*pymain-call-lock*)
           (push (apply #'list* fun args) *pymain-call-stack*)
           (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
           (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
           (values-list (ensure-no-error (pop *pymain-result-stack*)))))))
