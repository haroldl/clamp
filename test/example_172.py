import import_value
import import_pkg
import import_pkg.sub

for spec in [import_value.__spec__, import_pkg.__spec__, import_pkg.sub.__spec__]:
    print(spec.parent)

value_spec = import_value.__spec__
value_spec.name = "renamed.child"
print(value_spec.parent)
value_spec.submodule_search_locations = []
print(value_spec.parent)
value_spec.submodule_search_locations = None
print(value_spec.parent)

pkg_spec = import_pkg.__spec__
pkg_spec.submodule_search_locations = None
print(pkg_spec.parent)
pkg_spec.name = "renamed.package"
print(pkg_spec.parent)
pkg_spec.submodule_search_locations = []
print(pkg_spec.parent)
