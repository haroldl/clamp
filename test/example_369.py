import importlib.machinery as m
import importlib.util as u
import sys

suffix = ".cpython-312-x86_64-linux-gnu.so"
init_path = "/tmp/clamp_ext_pkg/__init__" + suffix
mod_path = "/tmp/clamp_ext_pkg/sub" + suffix
writer = m.SourceFileLoader("writer", "/tmp/writer.py")
writer.set_data(init_path, b"not a real extension")
writer.set_data(mod_path, b"not a real extension")

init_spec = u.spec_from_file_location("clamp_ext_pkg", init_path)
print(type(init_spec.loader).__name__)
print(init_spec.loader.is_package("clamp_ext_pkg"))
print(init_spec.cached is None)
print(type(init_spec.submodule_search_locations).__name__)
print(init_spec.submodule_search_locations[0].endswith("/clamp_ext_pkg"))
print(init_spec.parent)

mod_spec = u.spec_from_file_location("clamp_ext_pkg.sub", mod_path)
print(mod_spec.loader.is_package("clamp_ext_pkg.sub"))
print(mod_spec.submodule_search_locations is None)
print(mod_spec.parent)

sys.path.insert(0, "/tmp")
found_pkg = u.find_spec("clamp_ext_pkg")
print(type(found_pkg.loader).__name__)
print(found_pkg.origin.endswith("/clamp_ext_pkg/__init__" + suffix))
print(type(found_pkg.submodule_search_locations).__name__)
print(found_pkg.submodule_search_locations[0].endswith("/clamp_ext_pkg"))

finder = m.FileFinder("/tmp", (m.ExtensionFileLoader, m.EXTENSION_SUFFIXES))
file_pkg = finder.find_spec("clamp_ext_pkg")
print(type(file_pkg.loader).__name__)
print(file_pkg.origin.endswith("/clamp_ext_pkg/__init__" + suffix))
print(type(file_pkg.submodule_search_locations).__name__)
print(file_pkg.submodule_search_locations[0].endswith("/clamp_ext_pkg"))

writer.set_data("/tmp/clamp_both.py", b"VALUE = 'module'\n")
writer.set_data("/tmp/clamp_both/__init__.py", b"VALUE = 'package'\n")
both_spec = u.find_spec("clamp_both")
print(both_spec.origin.endswith("/clamp_both/__init__.py"))
print(type(both_spec.submodule_search_locations).__name__)
