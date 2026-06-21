import importlib.util
import importlib.machinery as m
import os
import sys

root = "/tmp/clamp_ns_pkg_example_365"
os.makedirs(root + "/ns_pkg", exist_ok=True)
m.SourceFileLoader("fixture", "/tmp/fixture.py").set_data(root + "/ns_pkg/mod.py", b"value = 7\n")
sys.path.insert(0, root)

spec = importlib.util.find_spec("ns_pkg")
print(spec is None)
print(spec.name)
print(spec.loader is None)
print(spec.origin)
print(spec.has_location)
print(type(spec.submodule_search_locations).__name__)
print(spec.submodule_search_locations[0].endswith("/ns_pkg"))
print(hasattr(m, "NamespaceLoader"))

import ns_pkg
print(type(ns_pkg).__name__)
print(getattr(ns_pkg, "__file__", None) is None)
print(ns_pkg.__path__[0].endswith("/ns_pkg"))
print(type(ns_pkg.__loader__).__name__)
print(ns_pkg.__spec__.loader is None)
import ns_pkg.mod
print(ns_pkg.mod.value)
