import importlib.machinery as m
import importlib.util

spec = importlib.util.find_spec("_json")
loader = m.ExtensionFileLoader("_json", spec.origin)
print(hasattr(loader, "get_code"))
print(loader.get_code("_json") is None)
print(hasattr(loader, "get_source"))
print(loader.get_source("_json") is None)
print(loader.is_package("_json"))
print(len(loader.get_data(spec.origin)) > 0)

pkg_loader = m.ExtensionFileLoader("pkg", "/tmp/pkg/__init__.cpython-312-x86_64-linux-gnu.so")
mod_loader = m.ExtensionFileLoader("pkg.mod", "/tmp/pkg/mod.cpython-312-x86_64-linux-gnu.so")
print(pkg_loader.is_package("pkg"))
print(mod_loader.is_package("pkg.mod"))
