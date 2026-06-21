import importlib
import sys

print(hasattr(importlib, 'util'))
print(hasattr(importlib, 'machinery'))
spec = importlib.util.find_spec('_json')
print(spec.name)
print(type(spec.loader).__name__)
print(spec.loader.name)
print(spec.loader.path.endswith(tuple(importlib.machinery.EXTENSION_SUFFIXES)))
created = spec.loader.create_module(spec)
print(created.__name__)
print(created.__spec__ is None)
print(created is sys.modules.get('_json'))
import _json
print(importlib.reload(_json) is _json)
print(sys.modules['_json'] is _json)
