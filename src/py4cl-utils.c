// Compile using: gcc -I/media/common-storage/miniconda3/include/python3.8 -c -Wall -Werror -fpic py4cl-utils.c && gcc -shared -o libpy4cl-utils.so py4cl-utils.o

#include <Python.h>
#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <ndarraytypes.h>
#include <arrayobject.h>

const char* PyTypeObject_Name(PyTypeObject* o){return o->tp_name;}
const char* PyTypeObject_Doc(PyTypeObject* o){return o->tp_doc;}

void** import_numpy(){
  _import_array();
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
  default: return "";
  }
};

void* PyArray_Data(PyArrayObject* arr){return PyArray_DATA(arr);}
