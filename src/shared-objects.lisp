(in-package :py4cl2-cffi)

(defvar *utils-source-file-path*
  (merge-pathnames
   (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
   #p"py4cl-utils.c"))

(defvar *utils-shared-object-path*
  (merge-pathnames
   (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
   #p"libpy4cl-utils.so"))

(defun compile-utils-shared-object ()
  (uiop:with-current-directory ((asdf:component-pathname (asdf:find-system "py4cl2-cffi")))
    (let ((program-string
            (format nil
                    ;; /media/common-storage/miniconda3/include/python3.8
                    "gcc -I'~A' -I'~Anumpy/' -c -Wall -Werror -fpic py4cl-utils.c && gcc -shared -o libpy4cl-utils.so py4cl-utils.o"
                    (namestring *python-include-path*)
                    (string-trim (list #\newline)
                                 (namestring
                                  (asdf:component-pathname
                                   (asdf:find-system "py4cl2-cffi")))))))
      (write-line program-string)
      (uiop:run-program program-string
                        :error-output *error-output*
                        :output *standard-output*))))

(compile-utils-shared-object)
