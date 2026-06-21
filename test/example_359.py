import importlib.machinery as m
import importlib.util
import os
from pathlib import Path

json_spec = importlib.util.find_spec("_json")
origin_dir = os.path.dirname(json_spec.origin)
loader = m.ExtensionFileLoader("_json", json_spec.origin)
finder = m.FileFinder(origin_dir, (m.ExtensionFileLoader, m.EXTENSION_SUFFIXES))
spec = finder.find_spec("_json")
hook = m.FileFinder.path_hook((m.ExtensionFileLoader, m.EXTENSION_SUFFIXES))
hooked_spec = hook(origin_dir).find_spec("_json")

print(hasattr(m, "FileFinder"))
print(hasattr(m, "SOURCE_SUFFIXES"))
print(hasattr(m, "BYTECODE_SUFFIXES"))
print(hasattr(m, "all_suffixes"))
print(type(loader).__name__)
print(loader.name)
print(loader.path == json_spec.origin)
print(loader.get_filename("_json") == json_spec.origin)
print(type(finder).__name__)
print(spec.name)
print(type(spec.loader).__name__)
print(spec.origin == json_spec.origin)
print(importlib.util.spec_from_file_location("_json_pathlike", Path(json_spec.origin)).origin == json_spec.origin)
print(finder.find_spec("_missing_clamp_native_ext_") is None)
print(callable(hook))
print(hooked_spec.name)
print(type(hooked_spec.loader).__name__)
try:
    hook("/definitely/missing/clamp/path")
except ImportError as exc:
    print(type(exc).__name__)
try:
    loader.get_filename("_decimal")
except ImportError as exc:
    print(type(exc).__name__, "cannot handle" in str(exc))
print(any(s in m.all_suffixes() for s in m.EXTENSION_SUFFIXES))
