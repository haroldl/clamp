import import_value
import import_pkg

value_loader = import_value.__loader__
pkg_loader = import_pkg.__loader__

print(type(hash(value_loader)).__name__)
print(hash(value_loader) == value_loader.__hash__())
print(hash(value_loader) == hash(value_loader))
print(hash(value_loader) == hash(pkg_loader))

pkg_loader.name = value_loader.name
pkg_loader.path = value_loader.path
print(value_loader == pkg_loader)
print(hash(value_loader) == hash(pkg_loader))

before = hash(pkg_loader)
pkg_loader.path = "changed.py"
print(hash(pkg_loader) == pkg_loader.__hash__())
print(hash(pkg_loader) == before)
