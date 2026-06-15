import import_value
import import_pkg
import import_pkg.sub

print(import_value.__cached__.endswith("test/__pycache__/import_value.cpython-314.pyc"))
print(import_pkg.__cached__.endswith("test/import_pkg/__pycache__/__init__.cpython-314.pyc"))
print(import_pkg.sub.__cached__.endswith("test/import_pkg/__pycache__/sub.cpython-314.pyc"))
