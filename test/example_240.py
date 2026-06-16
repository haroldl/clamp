import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files / "sub.py"
nested = files / "nested" / "resource.txt"
init = files / "__init__.py"
dotted = files / "archive.tar.gz"
hidden = files / ".resource"
dots = files / "..."
root = files.joinpath("/")

print(sub.stem)
print(nested.stem)
print(init.stem)
print(dotted.stem)
print(hidden.stem)
print(dots.stem)
print(files.stem)
print(root.stem == "")
