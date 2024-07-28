clamp
=====

Parsing Python
--------------

Use the built-in Python AST parsing module. Invoke Python from Common Lisp (SBCL) using C API bindings.

From https://www.linuxjournal.com/article/8497

Docs are here: https://docs.python.org/3/library/ast.html

Run `python-config` for compile/link flags if needed.

```
Py_Initialize();
PyRun_SimpleString(code);
Py_Finalize();
```

Implement in Python the translation to Common Lisp, have the Lisp system eval the result of the Python translation code.

SBCL Foreign Function Interface:
http://www.sbcl.org/manual/index.html#Foreign-Function-Interface

Lisp-1 versus Lisp-2
--------------------

Just treat Python functions as normal variables and always use #'apply or #'funcall to invoke them in the generated Lisp code.
