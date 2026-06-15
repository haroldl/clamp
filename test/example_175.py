import import_value
import import_pkg
import import_pkg.sub

for module in [import_value, import_pkg, import_pkg.sub]:
    print(module.__repr__() == repr(module))
    print(module.__repr__().startswith("<module '" + module.__name__ + "' from "))
    print(module.__repr__().endswith(module.__file__ + "'>"))

for loader in [
    import_value.__loader__,
    import_pkg.__loader__,
    import_pkg.sub.__loader__,
]:
    print(loader.__repr__() == repr(loader))
    print(loader.__repr__().startswith("<_frozen_importlib_external.SourceFileLoader object"))

for spec in [import_value.__spec__, import_pkg.__spec__, import_pkg.sub.__spec__]:
    print(spec.__repr__() == repr(spec))
    print(spec.__repr__().startswith("ModuleSpec(name='" + spec.name + "', loader="))
