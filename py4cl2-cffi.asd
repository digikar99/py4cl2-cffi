(defsystem "py4cl2-cffi/config"
  :pathname #P"src/"
  :depends-on ("uiop")
  :serial t
  :components ((:file "config")))

(defsystem "py4cl2-cffi"
  :pathname #P"src/"
  :description "CFFI based alternative to PY4CL2, primarily developed for performance reasons."
  :author "Shubhamkar Ayare <shubhamayare@yahoo.co.in>"
  :license "MIT"
  :version "0.1.0" ; beta
  :depends-on ("bordeaux-threads"
               "cffi"
               "cl-ppcre"
               "uiop"
               "alexandria"
               "trivial-garbage"
               "optima"
               "iterate"
               "float-features"
               "parse-number"
               "py4cl2-cffi/config"
               "trivial-backtrace")
  :serial t
  :components ((:static-file "py4cl.py")
               (:file "package")
               (:file "lisp-utils")
               (:static-file "py4cl-utils.c")
               (:file "shared-objects")
               (:static-file "libpy4cl-utils.so")
               (:file "gil-gc")
               (:file "numpy")
               (:file "features")
               (:file "pythonizers")
               (:file "python-process")
               (:file "lispifiers")
               (:file "py-repr")
               (:file "callpython")
               (:file "arg-list")
               (:file "import-export")
               (:file "lisp-classes")
               (:file "do-after-load"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (handler-case (let ((system (asdf:find-system "py4cl2-cffi-tests")))
                             (asdf:test-system system))
               (asdf:missing-component (condition)
                 (declare (ignore condition))
                 (format *error-output* "Please find the tests at ~A~%"
                         "https://github.com/digikar99/py4cl2-cffi-tests")
                 (format *error-output*
                         "If you have already set up the tests, then something is wrong,
as asdf was unable to find \"py4cl2-cffi-tests\".")))))
