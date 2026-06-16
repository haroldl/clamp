import import_pkg

reader = import_pkg.__loader__.get_resource_reader(import_pkg.__name__)
files = reader.files()
joined = files.joinpath("sub.py")
missing = files.joinpath("missing.py")
print(type(files).__name__)
print(str(files).endswith("test/import_pkg"))
print(type(joined).__name__)
print(str(joined).endswith("test/import_pkg/sub.py"))
print(joined.name)
print(joined.is_file())
print(files.is_dir())
print(files.exists())
print(missing.exists())
seen_init = False
seen_sub = False
for child in files.iterdir():
    if child.name == "__init__.py":
        seen_init = True
    if child.name == "sub.py":
        seen_sub = True
print(seen_init)
print(seen_sub)
handle = joined.open("rb")
print(handle.read(5))
