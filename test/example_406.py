import importlib
import importlib.metadata as metadata
from importlib.metadata import Distribution, PackageNotFoundError, entry_points, version

print(metadata.__name__, type(metadata.__file__).__name__, hasattr(metadata, "__path__"))
for name in ["PackagePath", "Prepared", "FastPath", "Lookup", "MetadataPathFinder"]:
    value = getattr(metadata, name, None)
    print(name, type(value).__name__, value is not None)
print(Distribution.__name__, PackageNotFoundError.__name__, type(version("pip")).__name__, type(entry_points()).__name__)
for sub in ["_adapters", "_collections", "_functools", "_itertools", "_meta", "_text"]:
    module = importlib.import_module("importlib.metadata." + sub)
    print(module.__name__)
