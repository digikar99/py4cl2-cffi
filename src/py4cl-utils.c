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

int PyArray_typenum_from_element_type(const char *element_type){
  // The alternative to this could be to parse the header file for the typenum
  // and then define them in lisp using CFFI
  if (strcmp(element_type, "f32") == 0)
    return NPY_FLOAT32;
  else if (strcmp(element_type, "f64") == 0)
    return NPY_FLOAT64;
  else if (strcmp(element_type, "sb64") == 0)
    return NPY_INT64;
  else if (strcmp(element_type, "ub64") == 0)
    return NPY_UINT64;
  else if (strcmp(element_type, "sb32") == 0)
    return NPY_INT32;
  else if (strcmp(element_type, "ub32") == 0)
    return NPY_UINT32;
  else if (strcmp(element_type, "sb16") == 0)
    return NPY_INT16;
  else if (strcmp(element_type, "ub16") == 0)
    return NPY_UINT16;
  else if (strcmp(element_type, "sb8") == 0)
    return NPY_INT8;
  else if (strcmp(element_type, "ub8") == 0)
    return NPY_UINT8;
  else
    return 0;
};
