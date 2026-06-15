# Python Import Implementation Plan

## Summary

Implement Python imports around first-class Clamp module objects and a `sys.modules`-style cache. Common Lisp packages are useful as isolated execution namespaces for generated module code, but they should not be the semantic model exposed to Python code.

## Design

- Represent modules as `py-module-object` values with normal Python attribute storage, `__name__`, `__package__`, `__file__`, and a hidden backing package name.
- Maintain a process-wide module cache keyed by full dotted names. Insert modules before execution and remove them if execution fails, following CPython importlib's important circular-import behavior.
- Resolve v1 source modules from the main script directory and current working directory. Support `module.py`, `package/__init__.py`, and `package/submodule.py`.
- Use Common Lisp packages only to isolate generated top-level symbols per module. Top-level assignments and function definitions also synchronize into the module object's attributes.

## Supported V1 Syntax

- `import m` and `import a.b`, including parent binding for dotted imports.
- `import m as alias` and `import a.b as alias`.
- `from m import name` and `from m import name as alias`.
- Package submodule imports when the package has `__init__.py`.

## Deferred Work

- Relative imports, star imports, namespace packages, full `sys.path`, custom import hooks, bytecode caches, extension modules, and zip imports.
- A user-visible `sys` module/dict API beyond the internal module cache.
- More precise local-scope import binding inside functions; imports inside functions should follow Python local/global scoping rather than relying on the current top-level-oriented binding path.
- Circular-import hardening, especially tests for partially initialized modules and `from ... import ...` during import cycles.

## Validation

Add examples covering plain imports, aliases, from-import, package submodules, and repeated imports executing once. Add circular-import examples before expanding import semantics further. Keep existing examples passing.
