(asdf:load-system "burgled-batteries3")

(in-package #:burgled-batteries3)

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

(startup-python)

(terpri)
(terpri)

(defmacro print-and-eval (form)
  (format t "Evaluating ~S~%" form)
  form)

(print-and-eval (defpyfun "str" (object)))

(defmacro print-and-eval-perf (n lambda-form)
  (terpri)
  (format t "Evaluating performance of~%  ~S~%on the basis of ~D runs..."
          lambda-form n)
  (force-output)
  `(format t "~%Calls per second: ~D~%" (calls-per-second ,n ,lambda-form)))

(print-and-eval-perf
 100000
 (lambda (x)
   (declare (optimize speed))
   (str x)))

(print-and-eval-perf
 100000
 (lambda (x)
   (declare (optimize speed))
   (str* x)))
