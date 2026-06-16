import import_value
import import_pkg

reader = import_value.__loader__.get_resource_reader(import_value.__name__)
namespace = reader.__dict__
print(namespace is reader.__dict__)
print("path" in namespace)
print(str(namespace["path"]).endswith("test"))
namespace["path"] = "changed"
print(reader.path)
print(str(reader.files()).endswith("changed"))
namespace["extra"] = "value"
print(reader.extra)
reader.path = import_pkg.__path__[0]
print(str(namespace["path"]).endswith("test/import_pkg"))
print(str(reader.files()).endswith("test/import_pkg"))
print("__dict__" in namespace)
