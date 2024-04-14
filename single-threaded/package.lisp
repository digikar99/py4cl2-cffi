(uiop:define-package :py4cl2-cffi/single-threaded
  (:use :cl :cffi :alexandria)
  (:reexport :py4cl2-cffi)
  (:import-from :py4cl2-cffi
                #:*internal-features*

                #:*pygc-threshold*
                #:with-pygc
                #:enable-pygc
                #:disable-pygc

                ;; #:pystart
                #:*additional-init-codes*
                ;; #:python-alive-p
                ;; #:python-start-if-not-alive
                #:with-python-output
                #:with-python-error-output
                ;; #:pystop
                ;; #:raw-pyeval
                ;; #:pyeval
                ;; #:raw-pyexec
                ;; #:pyexec
                #:pyerror
                ;; #:pyvalue

                #:pythonize
                #:pyobject-wrapper
                #:pyobject-wrapper-eq
                #:pyobject-wrapper-eq*
                #:define-lispifier

                #:*pythonizers*
                #:with-pythonizers
                #:*lispifiers*
                #:with-lispifiers

                #:chain*
                #:chain
                #:with-remote-objects
                #:with-remote-objects*

                #:*defpymodule-silent-p*
                #:*print-pyobject*
                #:*print-pyobject-wrapper-identity*

                #:+py-empty-tuple+
                #:+py-empty-tuple-pointer+
                #:+py-none+
                #:+py-none-pointer+)
  (:import-from :py4cl2-cffi
                #:thread-global-let
                #:%pystart

                #:*python-state*
                #:*utils-shared-object-path*
                #:*additional-init-codes*
                #:*getattr-ptr*
                #:*internal-features*
                #:*lisp-callback-fn-ptr*
                #:*numpy-c-api-pointer*
                #:*numpy-installed-p*
                #:*py-builtins-dict*
                #:*py-error-output-stream-pipe*
                #:*py-global-dict*
                #:*py-output-stream-pipe*
                #:*py-thread-state*
                #:*setattr-ptr*

                #:python-output-thread
                #:pyforeign-funcall
                #:with-python-gil
                #:mkfifo
                #:pytrack
                #:raw-py

                ))
