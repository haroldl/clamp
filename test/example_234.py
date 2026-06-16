import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
operator_path = files / "sub.py"
nested = files / "nested" / "resource.txt"
direct = files.__truediv__("__init__.py")

print(type(operator_path).__name__)
print(str(operator_path).endswith("test/import_pkg/sub.py"))
print(operator_path.name)
print(operator_path.is_file())
print(str(nested).endswith("test/import_pkg/nested/resource.txt"))
print(nested.name)
print(direct.name)
print(direct.read_text().splitlines()[0])
