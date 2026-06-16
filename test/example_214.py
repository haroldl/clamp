import import_value

loader = import_value.__loader__
namespace = loader.__dict__
print(namespace is loader.__dict__)
print(namespace["name"])
print(namespace["path"] == import_value.__file__)
namespace["name"] = "renamed_value"
namespace["path"] = "changed.py"
print(loader.name)
print(loader.path)
print(loader.get_filename("renamed_value"))
namespace["extra"] = "value"
print(loader.extra)
loader.path = "other.py"
print(namespace["path"])
print("name" in namespace)
print("__dict__" in namespace)
