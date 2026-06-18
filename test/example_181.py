import import_value
import import_pkg
import import_pkg.sub

for module in [import_value, import_pkg, import_pkg.sub]:
    spec = module.__spec__
    print(module.__cached__ is None)
    print(spec.cached is None)
    print(spec._cached is None)
    spec.cached = "custom-cache"
    print(spec.cached)
    print(spec._cached)
    spec.cached = None
    spec.has_location = False
    print(spec.cached is None)
    print(spec._cached is None)
    spec.has_location = True
    print(spec.cached is None)
    spec.origin = None
    spec.cached = None
    print(spec.cached is None)
