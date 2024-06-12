(asdf:load-system "py4cl2-cffi")

(defpackage :py4cl2-cffi-user
  (:use :cl :py4cl2-cffi)
  (:import-from #:py4cl2-cffi
                #:pyforeign-funcall))

(in-package :py4cl2-cffi-user)

(defun call-n-times (n function)
  (let ((start-time (get-internal-real-time)))
    (py4cl2-cffi::with-python-gil
      (dotimes (i n)
        (funcall function i)))
    (/ (- (get-internal-real-time) start-time)
       internal-time-units-per-second)))

(defun calls-per-second (n fn)
  (let* ((total-time    (call-n-times n fn))
         (per-call-time (/ total-time n)))
    (/ 1.0 per-call-time)))

(defun pystr (i)
  (declare (optimize speed)
           (type (signed-byte 64) i))
  (with-pygc
    (cffi:foreign-string-to-lisp
     (pyforeign-funcall "PyObject_Str"
                        :pointer (pythonize i)
                        :pointer))))

(defmacro print-and-eval-perf (n lambda-form)
  (terpri)
  (format t "Evaluating performance of~%  ~S~%on the basis of ~D runs..."
          lambda-form n)
  (force-output)
  `(format t "~%Calls per second: ~D~%" (calls-per-second ,n ,lambda-form)))

(pystart)

(print-and-eval-perf
 1000000
 (lambda (x)
   (declare (optimize speed))
   (pystr x)))

(print-and-eval-perf
 1000000
 (lambda (x)
   (declare (optimize speed))
   (pycall "str" x)))
