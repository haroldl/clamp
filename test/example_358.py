import importlib.machinery as m
import importlib.util
import os
spec = m.PathFinder.find_spec("_json")
origin_dir = os.path.dirname(importlib.util.find_spec("_json").origin)
path_spec = m.PathFinder.find_spec("_json", [origin_dir])
print(hasattr(m, "PathFinder"))
print(spec.name)
print(type(spec.loader).__name__)
print(path_spec.name)
print(type(path_spec.loader).__name__)
print(m.PathFinder.find_spec("_missing_clamp_native_ext_") is None)
