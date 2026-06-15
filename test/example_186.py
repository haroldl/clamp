import import_value
import import_pkg
import import_pkg.sub

for module in [import_value, import_pkg, import_pkg.sub]:
    loader = module.__loader__
    data = loader.get_data(module.__file__)
    print(type(data).__name__)
    print(len(data))
    print(data[0])
    print(data[-1])
    print(data[:5])

print(import_value.__loader__.get_data(import_pkg.__file__)[:4])
