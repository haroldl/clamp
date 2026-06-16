import import_value
import import_pkg

reader = import_value.__loader__.get_resource_reader(import_value.__name__)
print(type(reader).__name__)
print(str(reader.path).endswith("test"))
print(reader.__init__(import_pkg.__loader__) is None)
print(str(reader.path).endswith("test/import_pkg"))
print(str(reader.files()).endswith("test/import_pkg"))
print(reader.resource_path("__init__.py").endswith("test/import_pkg/__init__.py"))
print(reader.is_resource("__init__.py"))
print(reader.is_resource("sub.py"))
