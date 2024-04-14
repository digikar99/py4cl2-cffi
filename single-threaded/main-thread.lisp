(in-package :py4cl2-cffi/single-threaded)

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
    (%py-fun-and-args o))
  (defun (setf pymain-call) (fun-and-args)
    (declare (optimize speed))
    (setf (%py-fun-and-args o) fun-and-args))
  (defun pymain-result ()
    (declare (optimize speed))
    (%py-result o))
  (defun (setf pymain-result) (result)
    (declare (optimize speed))
    (setf (%py-result o) result)))
