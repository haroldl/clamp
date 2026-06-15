import import_value
import import_pkg
import import_pkg.sub

value_loader = import_value.__loader__
print(value_loader.get_filename(value_loader.name) == import_value.__file__)
print(value_loader.get_filename(import_value.__name__).endswith("test/import_value.py"))

pkg_loader = import_pkg.__loader__
print(pkg_loader.get_filename(pkg_loader.name) == import_pkg.__file__)
print(pkg_loader.get_filename(import_pkg.__name__).endswith("test/import_pkg/__init__.py"))

sub_loader = import_pkg.sub.__loader__
print(sub_loader.get_filename(sub_loader.name) == import_pkg.sub.__file__)
print(sub_loader.get_filename(import_pkg.sub.__name__).endswith("test/import_pkg/sub.py"))
