import import_value
import import_pkg
import import_pkg.sub

loaded_value = import_value.__loader__.load_module(import_value.__name__)
print(loaded_value is import_value)
print(loaded_value.__name__)
print(loaded_value.__loader__ is import_value.__loader__)

loaded_pkg = import_pkg.__loader__.load_module(import_pkg.__name__)
print(loaded_pkg is import_pkg)
print(loaded_pkg.__name__)
print(loaded_pkg.__path__ is import_pkg.__path__)

loaded_sub = import_pkg.sub.__loader__.load_module(import_pkg.sub.__name__)
print(loaded_sub is import_pkg.sub)
print(loaded_sub.__name__)
print(loaded_sub.__loader__ is import_pkg.sub.__loader__)
