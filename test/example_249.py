import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files / "sub.py"

print(files.__fspath__().endswith("test/import_pkg"))
print(sub.__fspath__().endswith("test/import_pkg/sub.py"))
print(files.__fspath__() == str(files))
print(type(files.__fspath__()).__name__)
print(files.__fspath__() == files.as_posix())
