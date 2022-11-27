(defpackage :py4cl2/cffi
  (:use :cl :cffi :alexandria :py4cl2/cffi-config)
  (:export #:pystart
           #:python-alive-p
           #:python-start-if-not-alive
           #:pystop
           #:raw-py
           #:pyvalue
           #:pycall
           #:pyslot-value))

(in-package :py4cl2/cffi)
