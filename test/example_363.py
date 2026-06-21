import sys
import importlib.machinery as m

print(hasattr(sys, "meta_path"))
print(type(sys.meta_path).__name__)
print(any(item is m.BuiltinImporter for item in sys.meta_path))
print(any(item is m.FrozenImporter for item in sys.meta_path))
print(any(item is m.PathFinder for item in sys.meta_path))
print(hasattr(sys, "path_hooks"))
print(type(sys.path_hooks).__name__)
print(any(callable(hook) for hook in sys.path_hooks))
print(hasattr(sys, "path_importer_cache"))
print(type(sys.path_importer_cache).__name__)
print(hasattr(sys, "builtin_module_names"))
print(type(sys.builtin_module_names).__name__)
print("sys" in sys.builtin_module_names)
print("_json" in sys.builtin_module_names)
print(type(sys.version).__name__, len(sys.version) > 0)
print(type(sys.version_info).__name__, sys.version_info.major == sys.version_info[0])
print(type(sys.hexversion).__name__)
print(type(sys.api_version).__name__)
print(type(sys.executable).__name__)
print(type(sys.prefix).__name__)
print(type(sys.base_prefix).__name__)
print(type(sys.exec_prefix).__name__)
print(type(sys.base_exec_prefix).__name__)
print(type(sys.platform).__name__, len(sys.platform) > 0)
print(sys.implementation.name)
print(bool(sys.implementation.cache_tag))

finder = None
for hook in sys.path_hooks:
    try:
        finder = hook("test")
        break
    except ImportError:
        pass
print(type(finder).__name__)
print(finder.find_spec("example_1").name)
print(type(finder.find_spec("example_1").loader).__name__)
try:
    for hook in sys.path_hooks:
        hook("/definitely/missing/clamp/path")
except ImportError as exc:
    print(type(exc).__name__)
print(sys.path_importer_cache == {})
sys.path_importer_cache["test"] = finder
print(sys.path_importer_cache["test"] is finder)
