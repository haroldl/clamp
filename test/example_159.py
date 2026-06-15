import import_value
import import_pkg
import import_pkg.sub

print(import_value.__spec__.loader_state is None)
print(import_pkg.__spec__.loader_state is None)
print(import_pkg.sub.__spec__.loader_state is None)
