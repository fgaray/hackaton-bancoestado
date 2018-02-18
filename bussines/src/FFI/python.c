#include <python2.7/Python.h>
#include <stdio.h>

static void* (*haskell_function_dispatch)(void*, void*) = NULL;

void python_c_set_function(void* (*fun)(void*, void*)) {
    haskell_function_dispatch = fun;
}



PyObject *python_c_wrapper(PyObject *self, PyObject *args) {
    if (haskell_function_dispatch && args) {
        return haskell_function_dispatch((void*)self, (void*)args);
    } else {
        return NULL;
    }
}
