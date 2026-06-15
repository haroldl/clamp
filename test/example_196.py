import import_value

path = "/tmp/clamp_import_loader_set_data_missing/child/data.pyc"
loader = import_value.__loader__
data = loader.get_data(import_value.__file__)[:6]

print(loader.set_data(path, data))
print(loader.get_data(path))
print(loader.set_data("/tmp", data))
print("continued")
