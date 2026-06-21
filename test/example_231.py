import importlib.resources as resources
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
resource_files = resources.files(import_pkg)
resource_files_by_name = resources.files("import_pkg")
print(type(resource_files).__name__)
print(str(resource_files).endswith("test/import_pkg"))
print((resource_files / "sub.py").is_file())
print(str(resource_files / "nested" / "resource.txt").endswith("test/import_pkg/nested/resource.txt"))
print(str(resource_files_by_name).endswith("test/import_pkg"))
print(resources.read_text(import_pkg, "sub.py").splitlines()[0])
print(resources.read_text("import_pkg", "sub.py").splitlines()[0])
print(resources.read_binary(import_pkg, "sub.py")[:6])
with resources.as_file(resource_files / "sub.py") as file_path:
    print(type(file_path).__name__)
    print(str(file_path).endswith("test/import_pkg/sub.py"))
print(resources.is_resource(import_pkg, "sub.py"))
print(resources.is_resource("import_pkg", "sub.py"))
print("sub.py" in list(resources.contents(import_pkg)))
with resources.path(import_pkg, "sub.py") as legacy_path:
    print(type(legacy_path).__name__)
    print(str(legacy_path).endswith("test/import_pkg/sub.py"))
with resources.open_text(import_pkg, "sub.py") as legacy_text:
    print(legacy_text.read().splitlines()[0])
with resources.open_binary(import_pkg, "sub.py") as legacy_binary:
    print(legacy_binary.read()[:6])
