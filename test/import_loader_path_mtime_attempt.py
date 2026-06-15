import import_value

loader = import_value.__loader__
print(loader.path_mtime(import_value.__file__))
