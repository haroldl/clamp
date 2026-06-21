import pkgutil
import sys

root = "test/pkgutil_404_root"
sys.path.insert(0, root)
print(pkgutil.__name__, type(pkgutil.ModuleInfo).__name__)
importer = pkgutil.get_importer(root)
print(type(importer).__name__, sys.path_importer_cache[root] is importer)
manual = pkgutil.ModuleInfo(importer, "manual", False)
print(manual.name, manual[1], manual.ispkg)
mods = list(pkgutil.iter_modules([root]))
print(any(item.name == "pkgutil_404_pkg" and item.ispkg for item in mods))
walked = list(pkgutil.walk_packages([root]))
print(any(item.name == "pkgutil_404_pkg" and item.ispkg for item in walked))
print(any(item.name == "pkgutil_404_pkg.sub" and not item.ispkg for item in walked))
print(type(pkgutil.get_loader("pkgutil_404_pkg")).__name__)
print(type(pkgutil.find_loader("pkgutil_404_pkg.sub")).__name__)
print(pkgutil.get_data("pkgutil_404_pkg", "data.txt")[:7])
print(pkgutil.extend_path([root], "pkgutil_404_pkg") == [root])
