import import_pkg

reader = import_pkg.__loader__.get_resource_reader(import_pkg.__name__)
contents = reader.contents()
print(iter(contents) is contents)
entries = list(contents)
print("__init__.py" in entries)
print("sub.py" in entries)
print(next(contents, "done"))
