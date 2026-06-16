import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files / "sub.py"
nested = files / "nested" / "resource.txt"
init = files / "__init__.py"
dotted = files / "archive.tar.gz"
hidden = files / ".resource"
dots = files / "..."
extensionless = files / "README"

print(sub.suffixes)
print(nested.suffixes)
print(init.suffixes)
print(dotted.suffixes)
print(hidden.suffixes)
print(dots.suffixes)
print(extensionless.suffixes)
print(files.suffixes)
