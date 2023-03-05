(in-package :py4cl2-cffi)

(defmethod py-repr (object)
  (pycall "repr" object))

(defmethod py-repr ((object float))
  (switch (object :test #'eql)
    (float-features:double-float-nan "numpy.float64('nan')")
    (float-features:single-float-positive-infinity "numpy.float32('inf')")
    (float-features:single-float-negative-infinity "-numpy.float32('inf')")
    (float-features:double-float-positive-infinity "numpy.float64('inf')")
    (float-features:double-float-negative-infinity "-numpy.float64('inf')")
    (float-features:single-float-nan "numpy.float32('nan')")
    (t
     (etypecase object
       (single-float (format nil "numpy.float32(~A)" object))
       (double-float (format nil "numpy.float64(~A)"
                             (let* ((repr (write-to-string object))
                                    (dpos (position #\d repr)))
                               (when dpos (setf (char repr dpos) #\e))
                               repr)))))))

(defmethod py-repr ((object string))
  (cond ((find #\newline object)
         (concatenate 'string "\"\"\"" object "\"\"\""))
        ((ignore-errors (parse-number:parse-number object))
         (concatenate 'string "\"" object "\""))
        (t
         object)))

(defmethod py-repr ((object symbol))
  (pythonize-symbol object))

(defmethod py-repr ((object array))
  (let ((repr (pycall "repr" object)))
    (if (typep object 'simple-vector)
        repr
        (concatenate 'string "numpy.array" (subseq repr 5)))))
