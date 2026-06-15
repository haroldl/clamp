import import_value
import import_pkg

value_loader = import_value.__loader__
pkg_loader = import_pkg.__loader__

print(value_loader.__ne__(value_loader))
print(value_loader.__ne__(pkg_loader))
pkg_loader.name = value_loader.name
pkg_loader.path = value_loader.path
print(value_loader.__ne__(pkg_loader))
pkg_loader.extra = "pkg-only"
print(value_loader.__ne__(pkg_loader))
value_loader.extra = "pkg-only"
print(value_loader.__ne__(pkg_loader))

value_spec = import_value.__spec__
pkg_spec = import_pkg.__spec__

print(value_spec.__ne__(value_spec))
print(value_spec.__ne__(pkg_spec))
pkg_spec.name = value_spec.name
pkg_spec.loader = value_spec.loader
pkg_spec.origin = value_spec.origin
pkg_spec.submodule_search_locations = value_spec.submodule_search_locations
pkg_spec.cached = value_spec.cached
pkg_spec.has_location = value_spec.has_location
print(value_spec.__ne__(pkg_spec))
pkg_spec.cached = "different-cache"
print(value_spec.__ne__(pkg_spec))
