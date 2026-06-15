import import_value
import import_pkg

value_loader = import_value.__loader__
pkg_loader = import_pkg.__loader__

pkg_loader.name = value_loader.name
pkg_loader.path = value_loader.path
print(value_loader == pkg_loader)

pkg_loader.extra = "pkg-only"
print(value_loader == pkg_loader)

value_loader.extra = "pkg-only"
print(value_loader == pkg_loader)

value_loader.extra = "different"
print(value_loader == pkg_loader)

before = hash(pkg_loader)
pkg_loader.extra = "changed-but-hash-ignores-extra-attrs"
print(hash(pkg_loader) == before)
