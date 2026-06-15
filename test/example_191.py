import import_value

path = "/tmp/clamp_import_loader_cache_bytecode.tmp"
loader = import_value.__loader__
data = loader.get_data(import_value.__file__)[:7]
print(loader._cache_bytecode(import_value.__file__, path, data))
written = loader.get_data(path)
print(type(written).__name__)
print(len(written))
print(written)
