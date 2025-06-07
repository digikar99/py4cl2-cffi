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

(defun funcall/dedicated-thread (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply fun args))
        (t
         (setf (pymain-call) (cons fun args))
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pymain-result))))

(defun apply/dedicated-thread (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply #'apply fun args))
        (t
         (setf (pymain-call) (apply #'list* fun args))
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pymain-result))))
