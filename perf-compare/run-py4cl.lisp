(asdf:load-system "py4cl")

(defpackage :py4cl-user
  (:use :cl :py4cl))

(in-package :py4cl-user)

(defun call-n-times (n function)
  (let ((start-time (get-internal-real-time)))
    (dotimes (i n)
      (funcall function i))
    (/ (- (get-internal-real-time) start-time)
       internal-time-units-per-second)))

(defun calls-per-second (n fn)
  (let* ((total-time    (call-n-times n fn))
         (per-call-time (/ total-time n)))
    (/ 1.0 per-call-time)))

(defmacro print-and-eval-perf (n lambda-form)
  (terpri)
  (format t "Evaluating performance of~%  ~S~%on the basis of ~D runs..."
          lambda-form n)
  (force-output)
  `(format t "~%Calls per second: ~D~%" (calls-per-second ,n ,lambda-form)))

(python-start)

(print-and-eval-perf
 10000
 (lambda (x)
   (declare (optimize speed))
   (python-call "str" x)))

(print-and-eval-perf
 10000
 (lambda (x)
   (declare (optimize speed))
   (remote-objects
     (python-call "str" x))))
