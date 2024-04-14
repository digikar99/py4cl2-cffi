(uiop:define-package :py4cl2-cffi/single-threaded
  (:use :cl :cffi :alexandria)
  (:reexport :py4cl2-cffi)
  (:import-from :py4cl2-cffi
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
                #:+py-empty-tuple+
                #:+py-empty-tuple-pointer+
                #:+py-none+
                #:+py-none-pointer+

                #:python-output-thread
                #:pyforeign-funcall
                #:with-python-gil
                #:mkfifo
                #:pytrack
                #:raw-py))

(in-package :py4cl2-cffi/single-threaded)

(defvar *pymain-thread*)
(defvar *pymain-thread-fun-args-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-fun-args-semaphore"))
(defvar *pymain-thread-result-semaphore*
  (bt:make-semaphore :name "py4cl2-cffi-main-thread-result-semaphore"))

(defstruct (pymain-call-and-result (:conc-name %py-))
  (fun-and-args)
  (result))
(let ((o (make-pymain-call-and-result)))
  (defun pymain-call ()
    (declare (optimize speed))
    (%py-fun-and-args o))
  (defun (setf pymain-call) (fun-and-args)
    (declare (optimize speed))
    (setf (%py-fun-and-args o) fun-and-args))
  (defun pymain-result ()
    (declare (optimize speed))
    (%py-result o))
  (defun (setf pymain-result) (result)
    (declare (optimize speed))
    (setf (%py-result o) result)))

(defun funcall/single-threaded (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply fun args))
        (t
         (setf (pymain-call) (cons fun args))
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pymain-result))))

(defun apply/single-threaded (fun &rest args)
  (declare (optimize speed))
  (cond ((eq *pymain-thread* (bt:current-thread))
         (apply #'apply fun args))
        (t
         (setf (pymain-call) (apply #'list* fun args))
         (bt:signal-semaphore *pymain-thread-fun-args-semaphore*)
         (bt:wait-on-semaphore *pymain-thread-result-semaphore*)
         (pymain-result))))

(macrolet ((def (single-threaded-sym)
             (let* ((multi-threaded-sym
                      (find-symbol (symbol-name single-threaded-sym)
                                   :py4cl2-cffi))
                    (all-args (gensym "ARGS")))
               (when (and (not (member single-threaded-sym
                                       ;; Blacklisted symbols
                                       '(pystart mkfifo raw-py)))
                          (fboundp multi-threaded-sym)
                          (not (macro-function multi-threaded-sym)))
                 ;; FIXME: Handle keyword args
                 `(,(let ((args (swank/backend:arglist multi-threaded-sym)))
                      `(defun ,single-threaded-sym ,args
                         ,(multiple-value-bind
                                (required optional rest keyword other-keys)
                              (parse-ordinary-lambda-list args)
                            (if rest
                                `(apply/single-threaded
                                  (lambda (&rest ,all-args)
                                    (float-features:with-float-traps-masked t
                                      (apply #',multi-threaded-sym ,all-args)))
                                  ,@required
                                  ,@(mapcar #'first optional)
                                  ,rest)
                                `(funcall/single-threaded
                                  (lambda (&rest ,all-args)
                                    (float-features:with-float-traps-masked t
                                      (apply #',multi-threaded-sym ,all-args)))
                                  ,@required
                                  ,@(mapcar #'first optional))))))
                   ,(when (fboundp `(setf ,multi-threaded-sym))
                      (let ((args (swank/backend:arglist
                                   `(setf ,multi-threaded-sym))))
                        `(defun (setf ,single-threaded-sym) ,args
                           ,(multiple-value-bind
                                  (required optional rest keyword other-keys)
                                (parse-ordinary-lambda-list args)
                              (if rest
                                  `(apply/single-threaded
                                    (lambda (&rest ,all-args)
                                      (float-features:with-float-traps-masked t
                                        (apply #',multi-threaded-sym ,all-args)))
                                    ,@required
                                    ,@(mapcar #'first optional)
                                    ,rest)
                                  `(funcall/single-threaded
                                    (lambda (&rest ,all-args)
                                      (float-features:with-float-traps-masked t
                                        (apply #',multi-threaded-sym ,all-args)))
                                    ,@required
                                    ,@(mapcar #'first optional)))))))))))

           (def-all (&environment env)
             (let ((all nil))
               (do-external-symbols (s :py4cl2-cffi/single-threaded)
                 (nconcf all (macroexpand `(def ,s) env)))
               ;; Additional whitelisted names.
               (dolist (s `(py-module-pointer
                            load-python-and-libraries
                            mkfifo
                            pycall*
                            pyeval-save-thread
                            pygil-held-p
                            pyvalue*))
                 (nconcf all (macroexpand `(def ,s) env)))
               `(progn
                  ,@all))))

  (def-all))

(defun raw-py (cmd-char &rest code-strings)
  "CMD-CHAR should be #\e for eval and #\x for exec.

Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (python-start-if-not-alive)
  (with-python-gil
    (let* ((return-code nil)
           (command (ecase cmd-char
                      (#\e (apply #'concatenate 'string "_ = " code-strings))
                      (#\x (apply #'concatenate 'string code-strings))))
           (error-output
             (ecase *python-state*
               (:initialized
                (setq return-code
                      (pyforeign-funcall "PyRun_SimpleString" :string command :int)))
               (:initializing
                (setq return-code
                      (pyforeign-funcall "PyRun_SimpleString" :string command :int))
                ""))))
      (unless (zerop return-code)
        (error 'pyerror
               :format-control error-output))
      (ecase cmd-char
        (#\e (let ((ptr (pyvalue* "_")))
               (pyforeign-funcall "Py_IncRef" :pointer ptr)
               (pytrack ptr)))
        (#\x (values))))))

(defun pystart ()

  (when (and (eq *python-state* :initialized)
             (not (boundp '*pymain-thread*)))
    (error "It looks like PY4CL2-CFFI:PYSTART was called before
   PY4CL2-CFFI/SINGLE-THREADED:PYSTART
Please restart your lisp and call PY4CL2-CFFI/SINGLE-THREADED:PYSTART
  without calling PY4CL2-CFFI:PYSTART"))

  (when (eq *python-state* :initialized)
    (return-from pystart))

  (let ((*python-state* :initializing))

    (setq *pymain-thread*
          (bt:make-thread
           (lambda ()
             ;; Wait for input
             (print :waiting)
             (loop :do
               (bt:wait-on-semaphore *pymain-thread-fun-args-semaphore*)
               (destructuring-bind (fun &rest args)
                   (pymain-call)
                 ;; (setf (pymain-result) (apply fun args))
                 (handler-case (setf (pymain-result) (apply fun args))
                   (error (c)
                     (trivial-backtrace:print-backtrace c))))
               (bt:signal-semaphore *pymain-thread-result-semaphore*)))
           :name "py4cl2-cffi-python-main-thread"))

    (funcall/single-threaded #'%pystart))

  (setq *python-state* :initialized)

  t)
