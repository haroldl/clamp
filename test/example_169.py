import import_value
import import_pkg
import import_pkg.sub

for obj in (
    import_value.__loader__,
    import_pkg.__loader__,
    import_pkg.sub.__loader__,
):
    print(type(obj).__module__)
    print(type(obj).__name__)

for obj in (
    import_value.__spec__,
    import_pkg.__spec__,
    import_pkg.sub.__spec__,
):
    print(type(obj).__module__)
    print(type(obj).__name__)
