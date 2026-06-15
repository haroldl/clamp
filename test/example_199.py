import import_value
import import_pkg
import import_pkg.sub


def check(module, resource):
    reader = module.__loader__.get_resource_reader(module.__name__)
    handle = reader.open_resource(resource)
    print(type(handle).__name__)
    print(type(handle).__module__)
    first = handle.read(5)
    rest = handle.read()
    empty = handle.read()
    print(type(first).__name__)
    print(len(first))
    print(first[0])
    print(first[-1])
    print(len(rest) > 0)
    print(len(empty))
    print(handle.closed)
    print(handle.close())
    print(handle.closed)


check(import_value, "import_value.py")
check(import_pkg, "__init__.py")
check(import_pkg.sub, "sub.py")
