;;; probably, ASDF should have a feature for doing this
;;; this file should be called after loading all the other files

(in-package :py4cl2-cffi)

;; For :ARRAYS, *INTERNAL-FEATURES* is also modified from PYSTART function.
(loop :initially (setf *internal-features* ())
      :for (feature-name . exclude-conditions) :in *feature-exclusion-alist*
      :if (not (or (and (functionp exclude-conditions)
                        (funcall exclude-conditions))
                   (and (listp exclude-conditions)
                        (intersection exclude-conditions *features*))))
        :do (push feature-name *internal-features*))
