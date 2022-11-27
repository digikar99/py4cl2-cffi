(in-package :py4cl2/cffi)

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
  `(foreign-funcall-pointer
    (foreign-funcall "ptr_idx"
                     :pointer *numpy-c-api-pointer*
                     :int (or (gethash ,name *numpy-function-index-table*)
                              (error "No such numpy CAPI function found! May be it is not registered?"))
                     :pointer)
                            (:convention :cdecl)
                            ,@args))
