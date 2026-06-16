import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
same = files.joinpath()
nested = files.joinpath("nested", "resource.txt")
absolute = files.joinpath("/tmp", "clamp-path.txt")

print(type(same).__name__)
print(str(same).endswith("test/import_pkg"))
print(same.name)
print(type(nested).__name__)
print(str(nested).endswith("test/import_pkg/nested/resource.txt"))
print(nested.name)
print(str(absolute) == "/tmp/clamp-path.txt")
