(in-package :py4cl2-cffi)

(defconstant +disable-pystop+ py4cl2-cffi/config:*disable-pystop*)
(defconstant +python-call-mode+ py4cl2-cffi/config:*python-call-mode*)

(deftype list-of (&rest types)
  (if types
      `(cons ,(first types) (list-of ,@(rest types)))
      'null))

(defmacro thread-global-let (bindings &body body)
  (let* ((bindings (mapcar (lambda (binding)
                             ;; Normalize the bindings
                             (etypecase binding
                               (symbol
                                (list binding nil))
                               ((list-of symbol)
                                (list (first binding) nil))
                               ((list-of symbol t)
                                binding)))
                           bindings))
         (variables (mapcar #'first bindings))
         (gensyms (alexandria:make-gensym-list (length variables))))
    `(let (,@(mapcar (lambda (var gensym)
                       `(,gensym ,var))
                     variables gensyms))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (binding)
                          `(setq ,@binding))
                        bindings)
              ,@body)
         ,@(mapcar (lambda (var gensym)
                     `(setq ,var ,gensym))
                   variables gensyms)))))

(defmacro do-subseq-until ((subseq-var sequence item &rest position-args
                            &key &allow-other-keys)
                           &body body)
  (check-type subseq-var symbol)
  (with-gensyms (old-item-pos new-item-pos)
    (once-only (item sequence)
      `(loop :with ,old-item-pos := 0
             :while ,old-item-pos
             :do (let* ((,new-item-pos (position ,item ,sequence
                                                :start ,old-item-pos
                                                ,@position-args))
                        (,subseq-var   (subseq ,sequence ,old-item-pos ,new-item-pos)))
                   ,@body
                   (setq ,old-item-pos (when ,new-item-pos
                                         (1+ ,new-item-pos))))))))

(defun mkfifo (path)
  (cffi:foreign-funcall "mkfifo" :string path :int #o0664))

(defmacro ensure-non-null-pointer (pointer
                                   &key (format-control nil format-control-p)
                                     format-arguments)
  (once-only (pointer)
    `(if (null-pointer-p ,pointer)
         ,(if format-control-p
              `(error 'pyerror
                      :format-control ,format-control
                      :format-arguments ,format-arguments)
              `(error 'pyerror))
         ,pointer)))

(defun condition-backtrace (condition)
  (with-output-to-string (s)
    (uiop:print-condition-backtrace condition :stream s)))

(defvar *verbosity-depth* 0)
(defmacro with-verbosity (message &body body)
  (with-gensyms (values)
    `(let ((,values))
       (when verbose
         (loop :repeat *verbosity-depth* :do (write-string "  " *error-output*))
         (format *error-output* ,message)
         (format *error-output* "... ")
         (force-output *error-output*))
       (setf ,values (multiple-value-list
                      (thread-global-let ((*verbosity-depth* (1+ *verbosity-depth*)))
                        ,@body)))
       (when verbose
         (format *error-output* "Done.~%"))
       (values-list ,values))))
