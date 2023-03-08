(in-package :py4cl2-cffi)

(defvar *internal-features* nil
  "A list of PY4CL2 features available on the system. (Support for :ARRAYS
requires numpy and is only determinable after python process has started.)

The list can include one or more of:

  :WITH-PYTHON-OUTPUT
  :TYPED-ARRAYS
")

(defparameter *feature-exclusion-alist*
  `((:with-python-output        :ccl :ecl)
    (:typed-arrays              :abcl)))

(defvar *warn-on-unavailable-feature-usage* t
  "If non-NIL, then issues a WARNING on use of a feature unavailable on the platform.
See PY4CL2:*INTERNAL-FEATURES* for the list of available features.")
