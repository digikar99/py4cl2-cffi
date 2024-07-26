(in-package :py4cl2-cffi)

(defmethod py-repr (object)
  (pycall "repr" object))

(defmethod py-repr ((float float))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (cond ((float-features:float-nan-p float)
         (etypecase float
           (single-float "numpy.float32('nan')")
           (double-float "numpy.float64('nan')")
           (long-float "numpy.longdouble('nan')")))
        ((float-features:float-infinity-p float)
         (cond ((< 0 float)
                (etypecase float
                  (single-float "numpy.float32('inf')")
                  (double-float "numpy.float64('inf')")
                  (long-float "numpy.longdouble('inf')")))
               ((> 0 float)
                (etypecase float
                  (single-float "-numpy.float32('inf')")
                  (double-float "-numpy.float64('inf')")
                  (long-float "-numpy.longdouble('inf')")))))
        (t
         (etypecase float
           (single-float (format nil "numpy.float32(~A)" float))
           (double-float (format nil "numpy.float64(~A)"
                                 (let* ((repr (write-to-string float))
                                        (dpos (or (position #\d repr)
                                                  (position #\D repr))))
                                   (when dpos (setf (char repr dpos) #\e))
                                   repr)))
           (long-float (format nil "numpy.longdouble(\"~A\")"
                               (let* ((repr (write-to-string float))
                                      (dpos (or (position #\l repr)
                                                (position #\L repr))))
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
        (ppcre:regex-replace `(:sequence #\=
                                         (:register (:non-greedy-repetition
                                                     0 nil :everything))
                                         #\)
                               :end-anchor)
                             (concatenate 'string "numpy.array" (subseq repr 5))
                             "='\\1')"))))
