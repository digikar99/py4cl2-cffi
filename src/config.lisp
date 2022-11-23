(defpackage :py4cl2/cffi-config
  (:use :cl)
  (:export #:*python-shared-object-path*
           #:*python-include-path*
           #:*python-additional-libraries*
           #:*python-additional-libraries-search-path*))

(in-package :py4cl2/cffi-config)

;; Use python3-config or equivalent to discover these values

(declaim (type pathname
               *python-shared-object-path*
               *python-include-path*
               *python-additional-libraries-search-path*))
(defvar *python-shared-object-path*
  #P"/usr/lib/python3.8/config-3.8-x86_64-linux-gnu/libpython3.8.so")
(defvar *python-include-path* #P"/usr/include/python3.8/")
(defvar *python-additional-libraries* '("crypt" "m" "pthread" "dl" "rt"))
(defvar *python-additional-libraries-search-path* #P"/usr/lib/")



;; TODO: Could set up better defaults for different OS
