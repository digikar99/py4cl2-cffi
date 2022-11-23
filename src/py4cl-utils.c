// Compile using: gcc -I/media/common-storage/miniconda3/include/python3.8 -c -Wall -Werror -fpic py4cl-utils.c && gcc -shared -o libpy4cl-utils.so py4cl-utils.o

#include <Python.h>

const char* PyTypeObject_Name(PyTypeObject* o){return o->tp_name;}
const char* PyTypeObject_Doc(PyTypeObject* o){return o->tp_doc;}
