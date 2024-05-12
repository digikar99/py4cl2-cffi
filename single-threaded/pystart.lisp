(in-package :py4cl2-cffi/single-threaded)

(defun pystart ()

  (when (and (eq *python-state* :initialized)
             (not (boundp '*pymain-thread*)))
    (error "It looks like PY4CL2-CFFI:PYSTART was called before
   PY4CL2-CFFI/SINGLE-THREADED:PYSTART
Please restart your lisp and call PY4CL2-CFFI/SINGLE-THREADED:PYSTART
  without calling PY4CL2-CFFI:PYSTART"))

  (when (eq *python-state* :initialized)
    (return-from pystart))

  (thread-global-let ((*python-state* :initializing))

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
                     (condition-backtrace c))))
               (bt:signal-semaphore *pymain-thread-result-semaphore*)))
           :name "py4cl2-cffi-python-main-thread"))

    (funcall/single-threaded #'%pystart))

  (setq *python-state* :initialized)

  t)
