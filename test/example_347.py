import importlib
import sys

for name in ['_json', '_decimal']:
    if name in sys.modules:
        del sys.modules[name]
    print(name in sys.modules)
    spec = importlib.util.find_spec(name)
    print(spec.name)
    print(type(spec.loader).__name__)
    print(spec.origin.endswith(tuple(importlib.machinery.EXTENSION_SUFFIXES)))
    print(name in sys.modules)

json_path = importlib.util.find_spec('_json').origin
path_spec = importlib.util.spec_from_file_location('direct_json_ext', json_path)
print(path_spec.name)
print(type(path_spec.loader).__name__)
print(path_spec.origin.endswith(tuple(importlib.machinery.EXTENSION_SUFFIXES)))

spec = importlib.util.find_spec('_json')
module = importlib.util.module_from_spec(spec)
print(module.__name__)
print(module.__spec__.name)
print(type(module.__loader__).__name__)
print(callable(module.encode_basestring_ascii))
print(module.encode_basestring_ascii('x'))
