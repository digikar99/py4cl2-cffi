// Compile using: gcc -I/media/common-storage/miniconda3/include/python3.8 -c -Wall -Werror -fpic py4cl-utils.c && gcc -shared -o libpy4cl-utils.so py4cl-utils.o

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <numpy/ndarraytypes.h>
#include <numpy/arrayobject.h>

const char* PyTypeObject_Name(PyTypeObject* o){return o->tp_name;}
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

void** import_numpy(){
  import_array();
  return PyArray_API;
}

void* ptr_idx(void** ptr, int idx){return ptr[idx];}

PyArray_Descr* PyArray_Descr_from_element_type_code(const char *code){
  // The alternative to this could be to parse the header file for the typenum
  // and then define them in lisp using CFFI
  int typenum = 0;
  if (strcmp(code, "f32") == 0)
    typenum = NPY_FLOAT32;
  else if (strcmp(code, "f64") == 0)
    typenum = NPY_FLOAT64;
  else if (strcmp(code, "sb64") == 0)
    typenum = NPY_INT64;
  else if (strcmp(code, "ub64") == 0)
    typenum = NPY_UINT64;
  else if (strcmp(code, "sb32") == 0)
    typenum = NPY_INT32;
  else if (strcmp(code, "ub32") == 0)
    typenum = NPY_UINT32;
  else if (strcmp(code, "sb16") == 0)
    typenum = NPY_INT16;
  else if (strcmp(code, "ub16") == 0)
    typenum = NPY_UINT16;
  else if (strcmp(code, "sb8") == 0)
    typenum = NPY_INT8;
  else if (strcmp(code, "ub8") == 0)
    typenum = NPY_UINT8;
  else if (strcmp(code, "t") == 0)
    typenum = NPY_OBJECT;
  return PyArray_DescrFromType(typenum);
};


const char* PyArray_element_type_from_array(PyArrayObject* arr){
  // The alternative to this could be to parse the header file for the typenum
  // and then define them in lisp using CFFI
  PyArray_Descr* descr = PyArray_DESCR(arr);
  int typenum = descr->type_num;
  switch(typenum){
  case NPY_FLOAT32: return "CL:SINGLE-FLOAT";
  case NPY_FLOAT64: return "CL:DOUBLE-FLOAT";
  case NPY_INT64: return "(CL:SIGNED-BYTE 64)";
  case NPY_INT32: return "(CL:SIGNED-BYTE 32)";
  case NPY_INT16: return "(CL:SIGNED-BYTE 16)";
  case NPY_INT8:  return "(CL:SIGNED-BYTE 08)";
  case NPY_UINT64: return "(CL:UNSIGNED-BYTE 64)";
  case NPY_UINT32: return "(CL:UNSIGNED-BYTE 32)";
  case NPY_UINT16: return "(CL:UNSIGNED-BYTE 16)";
  case NPY_UINT8:  return "(CL:UNSIGNED-BYTE 08)";
  case NPY_OBJECT: return "CL:T";
  default: return "";
  }
};

void* PyArray_Data(PyArrayObject* arr){return PyArray_DATA(arr);}
void* PyArray_GetItem(PyArrayObject* arr, void* itemptr){
  return PyArray_GETITEM(arr, itemptr);
}
int PyArray_SetItem(PyArrayObject* arr, void* itemptr, PyObject* obj){
  return PyArray_SETITEM(arr, itemptr, obj);
}

const int PyArray_C_Contiguous = NPY_ARRAY_C_CONTIGUOUS;
const int PyArray_F_Contiguous = NPY_ARRAY_F_CONTIGUOUS;

int PyArray_Is_C_Contiguous(PyArrayObject* arr){
  return PyArray_IS_C_CONTIGUOUS(arr);
}
int PyArray_Is_F_Contiguous(PyArrayObject* arr){
  return PyArray_IS_F_CONTIGUOUS(arr);
}

PyObject* PY4CL_PyArray_FromArray(PyArrayObject *op, PyArray_Descr *newtype, int requirements){
  return PyArray_FromArray(op, newtype, requirements);
}
