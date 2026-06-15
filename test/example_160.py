import import_value
import import_pkg
import import_pkg.sub

for spec in (
    import_value.__spec__,
    import_pkg.__spec__,
    import_pkg.sub.__spec__,
):
    print(spec._set_fileattr is True)
    print(spec.has_location is True)
    print(spec._cached == spec.cached)
    print(spec._cached.endswith(".cpython-314.pyc"))
