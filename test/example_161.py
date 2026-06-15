import import_value
import import_pkg
import import_pkg.sub

value_loader = import_value.__loader__
print(type(value_loader).__name__)
print(value_loader.name)
print(value_loader.path == import_value.__file__)
print(value_loader is import_value.__spec__.loader)

pkg_loader = import_pkg.__loader__
print(type(pkg_loader).__name__)
print(pkg_loader.name)
print(pkg_loader.path == import_pkg.__file__)
print(pkg_loader is import_pkg.__spec__.loader)

sub_loader = import_pkg.sub.__loader__
print(type(sub_loader).__name__)
print(sub_loader.name)
print(sub_loader.path == import_pkg.sub.__file__)
print(sub_loader is import_pkg.sub.__spec__.loader)
