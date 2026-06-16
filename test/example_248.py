import import_pkg

reader = import_pkg.__loader__.get_resource_reader(import_pkg.__name__)
path = reader.files() / "sub.py"
data = import_pkg.__loader__.get_data(path)
stats = import_pkg.__loader__.path_stats(path)

print(len(data) == len(path.read_bytes()))
print(stats["size"] == len(data))
print(type(stats["mtime"]).__name__)
print(stats["mtime"] > 0.0)
