import import_value
import import_pkg
import import_pkg.sub


def check(module, resource):
    reader = module.__loader__.get_resource_reader(module.__name__)
    handle = reader.open_resource(resource)
    expected = reader.resource_path(resource)
    print(handle.name == expected)
    print(handle.name.endswith(resource))
    print(handle.name == handle.name)
    handle.close()
    print(handle.name == expected)


check(import_value, "import_value.py")
check(import_pkg, "__init__.py")
check(import_pkg.sub, "sub.py")
