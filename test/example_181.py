import import_value
import import_pkg
import import_pkg.sub

for module in [import_value, import_pkg, import_pkg.sub]:
    spec = module.__spec__
    suffix = module.__cached__[-35:]
    print(spec.cached.endswith(suffix))
    spec.cached = None
    print(spec._cached is None)
    print(spec.cached.endswith(suffix))
    print(spec._cached.endswith(suffix))
    spec.cached = None
    spec.has_location = False
    print(spec.cached is None)
    print(spec._cached is None)
    spec.has_location = True
    print(spec.cached.endswith(suffix))
    spec.origin = None
    spec.cached = None
    print(spec.cached is None)
