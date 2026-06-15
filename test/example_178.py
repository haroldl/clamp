import import_value
import import_pkg
import import_pkg.sub

for spec in [import_value.__spec__, import_pkg.__spec__, import_pkg.sub.__spec__]:
    print(spec.__hash__ is None)
