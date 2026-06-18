import importlib
import importlib.util
import importlib.machinery

module = importlib.import_module("import_value")
print(module.VALUE)
spec = importlib.util.find_spec("import_value")
print(spec.name)
print(spec.cached is None)
made = importlib.util.module_from_spec(spec)
print(made.__name__)
print(made.__cached__ is None)
file_spec = importlib.util.spec_from_file_location("direct_value", "test/import_value.py")
print(file_spec.name)
print(file_spec.cached is None)
loader = importlib.machinery.SourceFileLoader("direct_value", "test/import_value.py")
print(loader.get_filename("direct_value").endswith("test/import_value.py"))
print(importlib.invalidate_caches())
