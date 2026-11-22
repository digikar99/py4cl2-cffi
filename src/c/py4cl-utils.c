// Compile using: gcc -I/media/common-storage/miniconda3/include/python3.8 -c -Wall -Werror -fpic py4cl-utils.c && gcc -shared -o libpy4cl-utils.so py4cl-utils.o

#define PY_SSIZE_T_CLEAN
#include <Python.h>

const char* PyTypeObject_Name(PyTypeObject* o){return o->tp_name;}
const char* PyObject_TypeName(PyTypeObject* o){return Py_TYPE(o)->tp_name;}
const char* PyTypeObject_Doc(PyTypeObject* o){return o->tp_doc;}

Py_ssize_t Py_RefCnt(PyObject* o){return Py_REFCNT(o);}

void *lisp_callback_fn_ptr = NULL;

PyObject* (*getattr_ptr)(int, PyObject*) = NULL;
void (*setattr_ptr)(int, PyObject*, PyObject*) = NULL;

PyObject* getattr(int handle, PyObject* attr){
  return getattr_ptr(handle, attr);
}
void setattr(int handle, PyObject* attr, PyObject* value){
  setattr_ptr(handle, attr, value);
}

PyObject* LispCallback_helper(int handle, PyObject* args, PyObject* kwargs){
  PyObject* (*fn_ptr)(int, PyObject*, PyObject*);
  fn_ptr = lisp_callback_fn_ptr;
  return (*fn_ptr)(handle, args, kwargs);
}

void* ptr_idx(void** ptr, int idx){return ptr[idx];}

// Source: http://www.cse.yorku.ca/~oz/hash.html
unsigned long djb2_strhash(unsigned char *str){
  unsigned long hash = 5381;
  int c;

  while ((c = *str++))
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

  return hash;
}
