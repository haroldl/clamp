/*
  Provide a shim between the Clamp Common Lisp runtime and the
  Python-to-Common-Lisp compiler which is currently written in
  Python.

  This C code exists because the Python C API contains non-trivial
  structs (like PyObject) and function signatures that would need
  to be mapped into Common Lisp as alien types. Instead, the goal
  of this file is just to create as simple an API as possible for
  sending a string (Python code to be compiled) from Common Lisp
  to Python to be compiled and returned as a string (of Common
  Lisp code) to be evaluated.

  https://docs.python.org/3/c-api/index.html
 */

/* Must includes these before any standard includes: */
#define PY_SSIZE_T_CLEAN
#include <Python.h>

void clamp_shim_init() {
  Py_Initialize();
}

void clamp_shim_shutdown() {
  Py_Finalize();
}

/*
  For now let's just try stuff out...
 */
int main() {
  clamp_shim_init();
  printf("Hello, Pythonic world!\n");
  /* Call Py_DECREF(x) an any values returned from Python when done with the value. */
  clamp_shim_shutdown();
}
