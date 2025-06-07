(in-package :py4cl2-cffi)

(defvar *pymain-thread*)
(defvar *pymain-thread-fun-args-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-fun-args-semaphore"))
(defvar *pymain-thread-result-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-result-semaphore"))

(defvar *pymain-call-stack* nil)
(defvar *pymain-result-stack* nil)

(defun funcall/dedicated-thread (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply fun args))
        (t
         (push (cons fun args) *pymain-call-stack*)
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pop *pymain-result-stack*))))

(defun apply/dedicated-thread (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply #'apply fun args))
        (t
         (push (apply #'list* fun args) *pymain-call-stack*)
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pop *pymain-result-stack*))))
