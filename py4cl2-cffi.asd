(defsystem "py4cl2-cffi/config"
  :pathname #P"src/"
  :depends-on ("uiop")
  :serial t
  :components ((:file "config")))

(defsystem "py4cl2-cffi/config-darwin"
  :pathname #P"src/"
  :depends-on ("cl-ppcre" "py4cl2-cffi/config" "uiop")
  :serial t
  :components ((:file "config-darwin")))

(defsystem "py4cl2-cffi"
  :pathname #P"src/"
  :description "CFFI based alternative to PY4CL2, primarily developed for performance reasons."
  :author "Shubhamkar Ayare <shubhamayare@yahoo.co.in>"
  :license "MIT"
  :version "0.3.0" ; beta
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
               (:feature :darwin "py4cl2-cffi/config-darwin")
               "trivial-backtrace"
               "swank")
  :serial t
  :components ((:static-file "py4cl.py")
               (:file "package")
               (:file "lisp-utils")
               (:static-file "py4cl-utils.c")
               (:static-file "py4cl-numpy-utils.c")
               (:file "numpy-installed-p")
               (:file "shared-objects")
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

(defsystem "py4cl2-cffi/single-threaded"
  :depends-on ("py4cl2-cffi")
  :description "Certain libraries like matplotlib do not behave well in multithreaded environments. This system defines a package which contains shadowed symbols from py4cl2-cffi. These shadowed versions call python from a single main thread that is also responsible for importing all the libraries."
  :pathname #p"single-threaded/"
  :serial t
  :components ((:file "package")
               (:file "main-thread")
               (:file "single-threaded-wrappers")
               (:file "pystart")
               (:file "import-export")))
