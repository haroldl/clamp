import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files / "sub.py"
nested = files / "nested" / "resource.txt"
root = files.joinpath("/")

print(files.as_posix().endswith("test/import_pkg"))
print(sub.as_posix().endswith("test/import_pkg/sub.py"))
print(nested.as_posix().endswith("test/import_pkg/nested/resource.txt"))
print(root.as_posix())
print(files.as_posix() == str(files))
