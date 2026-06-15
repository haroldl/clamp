import import_value
import import_pkg
import import_pkg.sub

for module in [import_value, import_pkg, import_pkg.sub]:
    loader = module.__loader__
    print(loader.get_source(module.__name__).splitlines()[0])
    print(loader.is_package(module.__name__))
    loaded = loader.load_module()
    print(loaded is module)
    print(loaded.__name__)
