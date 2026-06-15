import import_value

path = "/tmp/clamp_import_loader_set_data.tmp"
loader = import_value.__loader__
data = loader.get_data(import_value.__file__)[:5]
print(loader.set_data(path, data))
written = loader.get_data(path)
print(type(written).__name__)
print(len(written))
print(written)
