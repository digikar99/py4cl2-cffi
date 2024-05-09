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
