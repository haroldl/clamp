import import_value
import import_pkg
import import_pkg.sub

print(import_value.__loader__.create_module(import_value.__spec__) is None)
print(import_pkg.__loader__.create_module(import_pkg.__spec__) is None)
print(import_pkg.sub.__loader__.create_module(import_pkg.sub.__spec__) is None)
