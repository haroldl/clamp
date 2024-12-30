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

static PyObject *clamp_python_globals;
static PyObject *clamp_python_locals;

void clamp_shim_init() {
  Py_Initialize();
  clamp_python_globals = PyDict_New();
  clamp_python_locals = PyDict_New();
  //clamp_shim_debug_print("globals", clamp_python_globals);
  //clamp_shim_debug_print("locals", clamp_python_locals);
}

void clamp_shim_shutdown() {
  Py_Finalize();
}

void clamp_shim_debug_print(const char *label, PyObject *pobj) {
  PyObject *pobjstr = PyObject_Str(pobj); /* Python: str(pobj) */
  /* printf("Pointer returned is %ld\n", (long) pobjstr); */
  Py_ssize_t out_size; /* optional: okay to also just pass NULL and not receive the length */
  const char *s = PyUnicode_AsUTF8AndSize(pobjstr, &out_size);
  /* printf("String pointer returned is %ld\n", (long) s); */
  printf("%s => %s\n\n", label, s);
}

/*
  Print out the exception stack trace if an error has occurred.
  Return true if there was an exception to print out, or false
  if there was no exception.

  https://docs.python.org/3/c-api/exceptions.html
*/
int clamp_shim_print_exception() {
  PyObject *exception_type = PyErr_Occurred();
  if (exception_type == NULL) {
    return 0;
  } else {
    PyErr_Print();
    return 1;
  }
}

PyObject * clamp_shim_eval_python(const char *python_code) {
  /*
    Use Py_single_input instead of Py_eval_input because the former persists the state
    between invocations of PyRun_String in the locals and the latter discards the state.
   */
  return PyRun_String(python_code, Py_single_input, clamp_python_globals, clamp_python_locals);
}

/*
  For now let's just try stuff out...

  TODO:
  - use PyRun_SimpleString to define a function
  - use PyRun_String to call the function and save the output
  - convert the result to a C string and printf it to stdout
 */
int main() {
  clamp_shim_init();
  printf("Hello, Pythonic world!\n\n");

  /*
  PyRun_SimpleString("def f(x):\n    return (2 * x + 1)");
  if (!clamp_shim_print_exception()) {
    printf("All good!\n\n");
  } else {
    return 1;
  }
  */

  PyObject *pobj = NULL;

  pobj = clamp_shim_eval_python("def f(x):\n    return (2 * x + 1)");
  if (!clamp_shim_print_exception()) {
    printf("All good!\n\n");
  } else {
    return 1;
  }

  clamp_shim_debug_print("pobj1", pobj);

  pobj = clamp_shim_eval_python("f(41 + 2)");
  if (!clamp_shim_print_exception()) {
    printf("All good!\n\n");
  } else {
    return 1;
  }

  clamp_shim_debug_print("pobj2", pobj);

  pobj = clamp_shim_eval_python("41 + 2");
  if (!clamp_shim_print_exception()) {
    printf("All good!\n\n");
  } else {
    return 1;
  }

  clamp_shim_debug_print("pobj3", pobj);

  /* Call Py_DECREF(x) an any values returned from Python when done with the value. */
  /* TODO: check the reference count to see if it is 0 */
  /* Py_DECREF(pobj); */

  clamp_shim_shutdown();
}
