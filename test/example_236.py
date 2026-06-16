import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files.joinpath("sub.py")
root = files.joinpath("/")

print(type(files.parent).__name__)
print(str(files.parent).endswith("test"))
print(files.parent.name)
print(str(sub.parent).endswith("test/import_pkg"))
print(sub.parent.name)
print(str(files.parent.joinpath("import_pkg")) == str(files))
print(str(root.parent))
print(root.parent is root)
