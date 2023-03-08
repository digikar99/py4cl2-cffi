(in-package :py4cl2-cffi)

(defvar *utils-source-file-path*
  (merge-pathnames
   (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
   #p"py4cl-utils.c"))

(defvar *utils-shared-object-path*
  (merge-pathnames
   (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
   #p"libpy4cl-utils.so"))

(defvar *numpy-installed-p*)

(eval-when (:compile-toplevel :load-toplevel)
  (let* ((numpy-installed-p-file
           (asdf:component-pathname
            (asdf:find-component
             "py4cl2-cffi" "numpy-installed-p")))
         (numpy-installed-p-old
           (read-file-into-string numpy-installed-p-file))
         (numpy-installed-p-new
           (format nil "CL:~A" (zerop (nth-value 2
                                                 (uiop:run-program
                                                  "python3 -c 'import numpy'"
                                                  :ignore-error-status t))))))
    (when (string/= numpy-installed-p-old
                    numpy-installed-p-new)
      (write-string-into-file numpy-installed-p-new numpy-installed-p-file
                              :if-exists :supersede
                              :if-does-not-exist :create))))

(defun compile-utils-shared-object ()
  (uiop:with-current-directory ((asdf:component-pathname (asdf:find-system "py4cl2-cffi")))
    (multiple-value-bind (numpy-path error-output error-status)
        (uiop:run-program
         "python3 -c 'import numpy; print(numpy.__path__[0])'"
         :output :string :ignore-error-status t)
      (declare (ignore error-output))
      (print (list numpy-path error-status))
      (let* ((numpy-installed-p
               (zerop error-status))
             (program-string
               (format nil
                       "gcc -I'~A' -I'~A' -c -Wall -Werror -fpic py4cl-utils.c && gcc -shared -o libpy4cl-utils.so py4cl-utils.o"
                       (namestring *python-include-path*)
                       (if numpy-installed-p
                           (format nil "~A/core/include/"
                                   (string-trim (list #\newline) numpy-path))
                           (namestring
                            (asdf:component-pathname
                             (asdf:find-system "py4cl2-cffi")))))))
        (setq *numpy-installed-p* numpy-installed-p)
        (write-line program-string)
        (uiop:run-program program-string
                          :error-output *error-output*
                          :output *standard-output*)))))

(compile-utils-shared-object)
