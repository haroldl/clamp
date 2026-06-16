import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files / "sub.py"

print(files.__str__().endswith("test/import_pkg"))
print(sub.__str__().endswith("test/import_pkg/sub.py"))
print(files.__str__() == str(files))
print(type(files.__str__()).__name__)
print([sub.__str__(), sub.name])
