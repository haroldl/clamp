import import_value
import import_pkg
import import_pkg.sub

def check(module, suffix):
    reader = module.__loader__.get_resource_reader(module.__name__)
    print(type(reader).__name__)
    print(type(reader).__module__)
    print(str(reader.path).endswith(suffix))
    print(str(reader.files()).endswith(suffix))
    print(reader.resource_path("data.txt").endswith(suffix + "/data.txt"))

check(import_value, "test")
check(import_pkg, "test/import_pkg")
check(import_pkg.sub, "test/import_pkg")
