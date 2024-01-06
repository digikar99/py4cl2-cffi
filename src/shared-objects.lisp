(in-package :py4cl2-cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *utils-source-file-path*
    (merge-pathnames
     (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
     #p"py4cl-utils.c"))

  (defvar *utils-shared-object-path*
    (merge-pathnames
     (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
     #p"libpy4cl-utils.so"))

  (defvar *numpy-installed-p*)

  (defun compile-utils-shared-object ()
    (uiop:with-current-directory ((asdf:component-pathname (asdf:find-system "py4cl2-cffi")))
      (multiple-value-bind (numpy-path error-output error-status)
          (uiop:run-program
           "cd ~/; python3 -c 'import numpy; print(numpy.__path__[0])'"
           :output :string :ignore-error-status t)
        (declare (ignore error-output))
        (let* ((numpy-installed-p
                 (zerop error-status))
               (program-string
                 (format nil
                         *python-compile-command*
                         (format nil "~{~a~^ ~}" *python-includes*)
                         (if numpy-installed-p
                             (format nil "~A/core/include/"
                                     (string-trim (list #\newline) numpy-path))
                             (namestring
                              (asdf:component-pathname
                               (asdf:find-system "py4cl2-cffi")))))))
          (setq *numpy-installed-p* numpy-installed-p)
          (format t "~&~A~%" program-string)
          (uiop:run-program program-string
                            :error-output *error-output*
                            :output *standard-output*))))))

(eval-when (:compile-toplevel)
  (compile-utils-shared-object))

(eval-when (:compile-toplevel :load-toplevel)
  (let* ((numpy-installed-p-file
           (asdf:component-pathname
            (asdf:find-component
             "py4cl2-cffi" "numpy-installed-p")))
         (numpy-installed-p-old
           (read-file-into-string numpy-installed-p-file))
         (numpy-installed-p (zerop (nth-value 2
                                              (uiop:run-program
                                               "python3 -c 'import numpy'"
                                               :ignore-error-status t))))
         (numpy-installed-p-new
           (format nil "CL:~A" numpy-installed-p)))
    (when (string/= numpy-installed-p-old
                    numpy-installed-p-new)
      (write-string-into-file numpy-installed-p-new numpy-installed-p-file
                              :if-exists :supersede
                              :if-does-not-exist :create))
    (setq *numpy-installed-p* numpy-installed-p)))

(defun load-python-and-libraries ()
  (labels ((ensure-directory-name (namestring)
             (if (ends-with-subseq "/" namestring :test #'char=)
                 namestring
                 (concatenate 'string namestring "/")))
           (libraries-and-search-paths (ldflags)
             ;; Following https://sourceware.org/pipermail/libc-alpha/2021-August/129718.html
             ;;   we will ignore lpthread, ldl, lutil, lanl
             ;; But we will also ignore lm
             (loop :with libraries := ()
                   :with search-paths := ()
                   :with unknown-flags := ()
                   :for flag :in ldflags
                   :do (cond ((< (length flag) 2)
                              (push flag unknown-flags))
                             ((member flag *python-ignore-ldflags*
                                      :test #'string=))
                             ((starts-with-subseq "-L" flag :test #'char=)
                              (push (ensure-directory-name (subseq flag 2))
                                    search-paths))
                             ((starts-with-subseq "-l" flag :test #'char=)
                              (push (%shared-library-from-ldflag flag)
                                    libraries)))
                   :finally (return (values (nreverse libraries)
                                            (nreverse search-paths))))))
    (multiple-value-bind (libraries search-paths)
        (libraries-and-search-paths *python-ldflags*)
      (mapc (lambda (library)
              (load-foreign-library library :search-path search-paths))
            libraries))
    (load-foreign-library *utils-shared-object-path*)
    (setq *python-libraries-loaded-p* t)))

#+cmucl
(load-python-and-libraries)
