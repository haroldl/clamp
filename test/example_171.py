import import_value
import import_pkg
import import_pkg.sub

for spec in [import_value.__spec__, import_pkg.__spec__, import_pkg.sub.__spec__]:
    print(spec.cached == spec._cached)
    spec.cached = "custom-cache"
    print(spec.cached)
    print(spec._cached)
    spec._cached = "private-cache"
    print(spec.cached)
    print(spec._cached)
    spec.has_location = False
    print(spec.has_location)
    print(spec._set_fileattr)
    spec._set_fileattr = True
    print(spec.has_location)
    print(spec._set_fileattr)
