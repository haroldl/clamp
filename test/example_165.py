import import_value
import import_pkg
import import_pkg.sub

print(import_pkg.__path__ is import_pkg.__spec__.submodule_search_locations)
import_pkg.__path__.append("extra-path")
print(import_pkg.__spec__.submodule_search_locations[-1])
print(len(import_pkg.__spec__.submodule_search_locations) == len(import_pkg.__path__))
print(import_value.__spec__.submodule_search_locations is None)
print(import_pkg.sub.__spec__.submodule_search_locations is None)
