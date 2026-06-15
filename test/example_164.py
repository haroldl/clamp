import import_value
import import_pkg
import import_pkg.sub

value_repr = repr(import_value.__spec__)
print(value_repr.startswith("ModuleSpec(name='import_value', loader="))
print("_frozen_importlib_external.SourceFileLoader object" in value_repr)
print("origin='" in value_repr)
print(value_repr.endswith("test/import_value.py')"))
print("submodule_search_locations" in value_repr)

pkg_repr = repr(import_pkg.__spec__)
print(pkg_repr.startswith("ModuleSpec(name='import_pkg', loader="))
print("origin='" in pkg_repr)
print("submodule_search_locations=[" in pkg_repr)
print(pkg_repr.endswith("test/import_pkg'])"))

sub_repr = repr(import_pkg.sub.__spec__)
print(sub_repr.startswith("ModuleSpec(name='import_pkg.sub', loader="))
print(sub_repr.endswith("test/import_pkg/sub.py')"))
print("submodule_search_locations" in sub_repr)
