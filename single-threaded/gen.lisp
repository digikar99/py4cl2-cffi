;;; sbcl --load ~/quicklisp/setup.lisp --script gen.lisp

;;; This file is expected to be run as a script. Some systems like ECL do not
;;; provide lambda lists functions and macros in all cases. Especially for them,
;;; we generate all the functions and macros beforehand.

(ql:quickload '(:py4cl2-cffi))

(load (asdf:component-pathname
       (asdf:find-component "py4cl2-cffi/single-threaded" "package")))

(defpackage :py4cl2-cffi/single-threaded-generator
  (:use :cl :alexandria))

(in-package :py4cl2-cffi/single-threaded-generator)

(defparameter *single-threaded-wrappers-file*
  (asdf:component-pathname
   (asdf:find-component "py4cl2-cffi/single-threaded" "single-threaded-wrappers")))

(defparameter *single-threaded-blacklist*
  '(pystart mkfifo raw-py define-lispifier with-lispifiers with-pythonizers
    defpyfun defpymodule)
  "List of function and macro names which will not be translated to their single-threaded.")

(defun call-form-from-fn-and-ll (fn lambda-list)
  (multiple-value-bind
        (required optional rest keywords)
      (parse-ordinary-lambda-list lambda-list)
    (cond (rest
           `(apply/single-threaded
             ,fn
             ,@required
             ,@(mapcar #'first optional)
             ,rest))
          (keywords
           `(funcall/single-threaded
             ,fn
             ,@required
             ,@(mapcar #'first optional)
             ,@(loop :for ((key name) init suppliedp) :in keywords
                     :nconcing (list (make-keyword key) name))))
          (t
           `(funcall/single-threaded
             ,fn
             ,@required
             ,@(mapcar #'first optional))))))

(defun sanitize-lambda-list (lambda-list)
  (let ((sanitized nil)
        (state :required))
    (dolist (item lambda-list)
      (cond ((member item lambda-list-keywords)
             (setq state item)
             (push item sanitized))
            (t
             (push
              (ecase state
                (:required item)
                (&optional item)
                (&key
                 (if (first item)
                     item
                     (if (keywordp item)
                         (intern (string item)
                                 :py4cl2-cffi/single-threaded)
                         item)))
                (&rest item))
              sanitized))))
    (nreverse sanitized)))

(defun expand-single-threaded-sym (single-threaded-sym)
  "Given a SYMBOL, returns an appropriate function or macro for single-threaded calling."
  (declare (optimize debug))
  (let* ((multi-threaded-sym
           (find-symbol (symbol-name single-threaded-sym)
                        :py4cl2-cffi))
         (args (gentemp "ARGS"))
         (macrop (macro-function multi-threaded-sym)))
    (when (and (not (member single-threaded-sym
                            *single-threaded-blacklist*))
               (fboundp multi-threaded-sym)
               (not (eq single-threaded-sym multi-threaded-sym)))
      ;; FIXME: Handle keyword args
      `(,(let ((lambda-list
                 (sanitize-lambda-list
                  (swank/backend:arglist multi-threaded-sym))))
           `(,(if macrop 'defmacro 'defun)
             ,single-threaded-sym ,lambda-list
             ,(call-form-from-fn-and-ll
               `(lambda (&rest ,args)
                  (float-features:with-float-traps-masked t
                    ,(if macrop
                         `(funcall (macro-function ',multi-threaded-sym)
                                   (list* ',multi-threaded-sym ,args) nil)
                         `(apply #',multi-threaded-sym ,args))))
               lambda-list)))
        ,(when (fboundp `(setf ,multi-threaded-sym))
           (let ((lambda-list
                   (sanitize-lambda-list
                    (swank/backend:arglist `(setf ,multi-threaded-sym)))))
             `(,(if macrop 'defmacro 'defun)
               (setf ,single-threaded-sym) ,lambda-list
               ,(call-form-from-fn-and-ll
                 `(lambda (&rest ,args)
                    (float-features:with-float-traps-masked t
                      ,(if macrop
                           `(apply (macro-function '(setf ,multi-threaded-sym))
                                   ,args)
                           `(apply #',multi-threaded-sym ,args))
                      (apply #'(setf ,multi-threaded-sym) ,args)))
                 lambda-list))))))))

(defun generate ()
  (let ((all nil))
    (do-external-symbols (s :py4cl2-cffi/single-threaded)
      (nconcf all (expand-single-threaded-sym s)))
    ;; Additional whitelisted names.
    (dolist (s `(py-module-pointer
                 load-python-and-libraries
                 mkfifo
                 pycall*
                 pyeval-save-thread
                 pygil-held-p
                 pyvalue*))
      (nconcf all (expand-single-threaded-sym s)))
    (with-open-file (*standard-output* *single-threaded-wrappers-file*
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
      (write-line ";;; This file was automatically generated by gen.lisp in the same directory.")
      (write-line ";;; Please do not edit this file manually.")
      (terpri)
      (let ((*print-case* :downcase))
        (write `(in-package :py4cl2-cffi/single-threaded))
        (dolist (form all)
          (terpri)
          (terpri)
          (write form))))))

(generate)
