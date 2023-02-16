(in-package :py4cl2-cffi)

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
