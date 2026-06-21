import importlib.machinery as m

loader = m.SourcelessFileLoader("pkg.mod", "/tmp/pkg/mod.pyc")
print(hasattr(m, "SourcelessFileLoader"))
print(type(loader).__name__)
print(loader.name)
print(loader.path)
print(loader.get_filename("pkg.mod"))
print(loader.get_source("pkg.mod"))
print(loader.is_package("pkg.mod"))
print(m.SourcelessFileLoader("pkg", "/tmp/pkg/__init__.pyc").is_package("pkg"))
try:
    loader.get_code("pkg.mod")
except Exception as exc:
    print("get_code", type(exc).__name__)
try:
    loader.get_data("/tmp/pkg/mod.pyc")
except Exception as exc:
    print("get_data", type(exc).__name__)

m.SourceFileLoader("fixture", "/tmp/fixture.py").set_data("/tmp/clamp_dummy_sourceless.pyc", b"not real pyc")
finder = m.FileFinder("/tmp", (m.SourcelessFileLoader, m.BYTECODE_SUFFIXES))
spec = finder.find_spec("clamp_dummy_sourceless")
print(spec.name)
print(type(spec.loader).__name__)
print(spec.origin.endswith("clamp_dummy_sourceless.pyc"))
print(spec.has_location)
print(finder.find_spec("missing_dummy_sourceless") is None)
