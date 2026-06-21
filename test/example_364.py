import importlib
import importlib.machinery as m
import sys

path_finder = m.PathFinder
file_finder = m.FileFinder("test", (m.SourceFileLoader, m.SOURCE_SUFFIXES))
for obj in [path_finder, file_finder]:
    print(hasattr(obj, "invalidate_caches"))
    print(obj.invalidate_caches())

sys.path_importer_cache["test"] = file_finder
print("test" in sys.path_importer_cache)
print(importlib.invalidate_caches())
print("test" in sys.path_importer_cache)
