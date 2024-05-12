#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <numpy/ndarraytypes.h>
#include <numpy/arrayobject.h>

void** import_numpy(){
  import_array();
  return PyArray_API;
}

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
  case NPY_FLOAT32: return "cl:single-float";
  case NPY_FLOAT64: return "cl:double-float";
  case NPY_INT64: return "(cl:signed-byte 64)";
  case NPY_INT32: return "(cl:signed-byte 32)";
  case NPY_INT16: return "(cl:signed-byte 16)";
  case NPY_INT8:  return "(cl:signed-byte 08)";
  case NPY_UINT64: return "(cl:unsigned-byte 64)";
  case NPY_UINT32: return "(cl:unsigned-byte 32)";
  case NPY_UINT16: return "(cl:unsigned-byte 16)";
  case NPY_UINT8:  return "(cl:unsigned-byte 08)";
  case NPY_OBJECT: return "cl:t";
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
