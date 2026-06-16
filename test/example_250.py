import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files / "sub.py"
renamed = sub.with_name("renamed.txt")
nested = (files / "nested" / "resource.txt").with_name("other.data")
root_child = files.joinpath("/tmp", "clamp-path.txt").with_name("changed.py")

print(type(renamed).__name__)
print(renamed.name)
print(renamed.suffix)
print(renamed.stem)
print(str(renamed).endswith("test/import_pkg/renamed.txt"))
print(str(nested).endswith("test/import_pkg/nested/other.data"))
print(root_child.name)
print(str(root_child))
