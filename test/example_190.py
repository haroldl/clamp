import import_value
import import_pkg
import import_pkg.sub

for module in [import_value, import_pkg, import_pkg.sub]:
    loader = module.__loader__
    stats = loader.path_stats(module.__file__)
    print(type(stats).__name__)
    print(len(stats))
    print(stats["size"] == len(loader.get_data(module.__file__)))
    print(stats["mtime"] > 0)
