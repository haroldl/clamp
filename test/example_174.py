import import_value
import import_pkg

value_spec = import_value.__spec__
pkg_spec = import_pkg.__spec__

print(value_spec == value_spec)
print(value_spec == pkg_spec)
print(value_spec != pkg_spec)
print(value_spec == None)

print(value_spec.loader == value_spec.loader)
print(value_spec.loader == pkg_spec.loader)
pkg_spec.loader.name = value_spec.loader.name
pkg_spec.loader.path = value_spec.loader.path
print(value_spec.loader == pkg_spec.loader)

pkg_spec.name = value_spec.name
pkg_spec.loader = pkg_spec.loader
pkg_spec.origin = value_spec.origin
pkg_spec.submodule_search_locations = value_spec.submodule_search_locations
pkg_spec.cached = value_spec.cached
pkg_spec.has_location = value_spec.has_location
print(value_spec == pkg_spec)

pkg_spec.cached = "different-cache"
print(value_spec == pkg_spec)
