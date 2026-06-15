import import_value
import import_pkg
import import_pkg.sub

for module in [import_value, import_pkg, import_pkg.sub]:
    stats = module.__loader__.path_stats(module.__file__)
    print(type(stats["mtime"]).__name__)
    print(stats["mtime"] > 0.0)
    print(stats["mtime"] < 2208988800.0)
