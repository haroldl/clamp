import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files / "sub.py"
nested = files / "nested" / "resource.txt"
init = files / "__init__.py"
dotted = files / "archive.tar.gz"
hidden = files / ".resource"
dots = files / "..."

print(sub.suffix)
print(nested.suffix)
print(init.suffix)
print(dotted.suffix)
print(hidden.suffix == "")
print(dots.suffix == "")
print(files.suffix == "")
