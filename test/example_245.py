import import_value
import import_pkg
import import_pkg.sub

for spec in [import_value.__spec__, import_pkg.__spec__, import_pkg.sub.__spec__]:
    namespace = spec.__dict__
    print(namespace is spec.__dict__)
    print(namespace["name"] == spec.name)
    print(namespace["loader"] is spec.loader)
    print(namespace["origin"] == spec.origin)
    print(namespace["loader_state"] is spec.loader_state)
    print(namespace["_cached"] == spec.cached)
    print(namespace["_set_fileattr"] == spec.has_location)

value_spec = import_value.__spec__
namespace = value_spec.__dict__
value_spec.name = "renamed_value"
print(namespace["name"], value_spec.name)
namespace["origin"] = "dict-origin.py"
print(value_spec.origin, namespace["origin"])
namespace["extra"] = "extra-value"
print(value_spec.extra, namespace["extra"])
