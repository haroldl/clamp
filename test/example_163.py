import import_value
import import_pkg
import import_pkg.sub

value_loader = import_value.__loader__
print(value_loader.is_package(import_value.__name__))

pkg_loader = import_pkg.__loader__
print(pkg_loader.is_package(import_pkg.__name__))

sub_loader = import_pkg.sub.__loader__
print(sub_loader.is_package(import_pkg.sub.__name__))
