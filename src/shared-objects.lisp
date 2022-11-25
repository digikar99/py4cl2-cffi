(in-package :py4cl2/cffi)

(defvar *utils-source-file-path*
  (merge-pathnames
   (asdf:component-pathname (asdf:find-system "py4cl2/cffi"))
   #p"py4cl-utils.c"))

(defvar *utils-shared-object-path*
  (merge-pathnames
   (asdf:component-pathname (asdf:find-system "py4cl2/cffi"))
   #p"libpy4cl-utils.so"))

(defvar *numpy-utils-shared-object-path*
  (merge-pathnames
   (asdf:component-pathname (asdf:find-system "py4cl2/cffi"))
   #p"libnumpy-utils.so"))

(defun compile-utils-shared-object ()
  (uiop:with-current-directory ((asdf:component-pathname (asdf:find-system "py4cl2/cffi")))
    (uiop:run-program
     (format nil
             ;; /media/common-storage/miniconda3/include/python3.8
             "gcc -I'~A' -I'~A'python3.8/site-packages/numpy/core/include/numpy/ -c -Wall -Werror -fpic py4cl-utils.c && gcc -shared -o libpy4cl-utils.so py4cl-utils.o"
             (namestring *python-include-path*)
             (namestring (directory-namestring *python-shared-object-path*)))
     :error-output *error-output*
     :output *standard-output*)))

(compile-utils-shared-object)
