import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files.joinpath("sub.py")

print(files.__repr__().startswith("PosixPath("))
print(files.__repr__().endswith("test/import_pkg')"))
print(files.__repr__() == repr(files))
print(sub.__repr__().endswith("test/import_pkg/sub.py')"))
