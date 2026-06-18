import import_value
import import_pkg
import import_pkg.sub

print(import_value.__cached__ is None)
print(import_pkg.__cached__ is None)
print(import_pkg.sub.__cached__ is None)
