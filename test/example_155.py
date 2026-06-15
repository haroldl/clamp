import import_pkg
print(type(import_pkg.__path__).__name__)
print(len(import_pkg.__path__))
print(import_pkg.__path__[0].endswith("test/import_pkg"))
import import_pkg.sub
print(import_pkg.sub.VALUE)
