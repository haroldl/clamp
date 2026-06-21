import importlib.machinery as m
import importlib.util
import sys

spec = m.BuiltinImporter.find_spec("sys")
missing_builtin = m.BuiltinImporter.find_spec("_json")
module = importlib.util.module_from_spec(spec)
print(hasattr(m, "BuiltinImporter"))
print(spec.name)
print(spec.origin)
print(spec.has_location)
print(spec.cached is None)
print(spec.submodule_search_locations is None)
print(spec.loader is m.BuiltinImporter)
print(missing_builtin is None)
print(m.BuiltinImporter.get_code("sys") is None)
print(m.BuiltinImporter.get_source("sys") is None)
print(m.BuiltinImporter.is_package("sys"))
print(module is sys)
print(module.__name__)
print(m.BuiltinImporter.load_module("sys") is sys)
try:
    m.BuiltinImporter.get_code("missing_clamp_builtin")
except ImportError as exc:
    print(type(exc).__name__)

print(hasattr(m, "FrozenImporter"))
print(m.FrozenImporter.find_spec("missing_clamp_frozen") is None)
try:
    m.FrozenImporter.get_code("missing_clamp_frozen")
except ImportError as exc:
    print(type(exc).__name__)
try:
    m.FrozenImporter.get_source("missing_clamp_frozen")
except ImportError as exc:
    print(type(exc).__name__)
try:
    m.FrozenImporter.is_package("missing_clamp_frozen")
except ImportError as exc:
    print(type(exc).__name__)
try:
    m.FrozenImporter.load_module("missing_clamp_frozen")
except ImportError as exc:
    print(type(exc).__name__)
