import import_value
import import_pkg
import import_pkg.sub

value_spec = import_value.__spec__
print(type(value_spec).__name__)
print(value_spec.name)
print(value_spec.parent)
print(value_spec.origin.endswith("test/import_value.py"))
print(value_spec.has_location)
print(value_spec.cached == import_value.__cached__)
print(value_spec.submodule_search_locations)
print(value_spec._initializing)

pkg_spec = import_pkg.__spec__
print(pkg_spec.name)
print(pkg_spec.parent)
print(pkg_spec.origin.endswith("test/import_pkg/__init__.py"))
print(type(pkg_spec.submodule_search_locations).__name__)
print(len(pkg_spec.submodule_search_locations))
print(pkg_spec.submodule_search_locations[0].endswith("test/import_pkg"))

sub_spec = import_pkg.sub.__spec__
print(sub_spec.name)
print(sub_spec.parent)
print(sub_spec.origin.endswith("test/import_pkg/sub.py"))
print(sub_spec.cached == import_pkg.sub.__cached__)
print(sub_spec.submodule_search_locations)
