import importlib

try:
    importlib.import_module(123)
except TypeError as exc:
    print(type(exc).__name__)
    print(str(exc))

try:
    importlib.reload("not a module")
except TypeError as exc:
    print(type(exc).__name__)
    print(str(exc))
