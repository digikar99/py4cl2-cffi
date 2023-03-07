(in-package :py4cl2-cffi)

(defvar *numpy-c-api-pointer*)

(defconstant +npy-array-c-contiguous+ #x0001)
(defconstant +npy-array-writeable+ #x0400)

;; Defined with respect to python 3.8, numpy 1.19.0
;; See numpy/core/include/numpy/__multiarray_api.h
(defparameter *numpy-function-index-table*
  (alist-hash-table
   '(("PyArray_DescrFromType" . 45)
     ("PyArray_NewFromDescr"  . 94))
   :test #'equal))

(defmacro numpy-funcall (name &rest args)
  `(with-python-gil
     (foreign-funcall-pointer
      (foreign-funcall "ptr_idx"
                       :pointer *numpy-c-api-pointer*
                       :int (or (gethash ,name *numpy-function-index-table*)
                                (error "No such numpy CAPI function found! May be it is not registered?"))
                       :pointer)
      (:convention :cdecl)
      ,@args)))

(defun cl-array-offset (array)
  (declare (optimize speed)
           (type cl:array array))
  (loop :with total-offset :of-type (signed-byte 61) := 0
        :if (typep array 'cl:simple-array)
          :do (return total-offset)
        :else
          :do (multiple-value-bind (displaced-to offset)
                  (cl:array-displacement array)
                (declare (type (signed-byte 61) offset))
                (incf total-offset offset)
                (setq array displaced-to))))

(defun array-storage (array)
  (declare (ignorable array)
           (optimize speed))
  (loop :with array := array
        :do (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
              (typecase array
                ((cl:simple-array * (*)) (return array))
                (cl:simple-array (return #+sbcl (sb-ext:array-storage-vector array)
                                         #+ccl (ccl::%array-header-data-and-offset array)
                                         #-(or sbcl ccl)
                                         (error "Don't know how to obtain ARRAY-STORAGE on ~S"
                                                (lisp-implementation-type))))
                (t (setq array (cl:array-displacement array)))))))
