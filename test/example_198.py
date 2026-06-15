import import_value
import import_pkg
import import_pkg.sub

root_reader = import_value.__loader__.get_resource_reader(import_value.__name__)
pkg_reader = import_pkg.__loader__.get_resource_reader(import_pkg.__name__)
sub_reader = import_pkg.sub.__loader__.get_resource_reader(import_pkg.sub.__name__)

root_contents = root_reader.contents()
pkg_contents = pkg_reader.contents()
sub_contents = sub_reader.contents()

print(root_reader.is_resource("import_value.py"))
print(root_reader.is_resource("import_pkg"))
print(root_reader.is_resource("missing_resource.py"))
print("import_value.py" in root_contents)
print("import_pkg" in root_reader.contents())
print(pkg_reader.is_resource("__init__.py"))
print(pkg_reader.is_resource("sub.py"))
print(pkg_reader.is_resource("missing.py"))
print("__init__.py" in pkg_contents)
print("sub.py" in pkg_contents)
print(sub_reader.is_resource("__init__.py"))
print(sub_reader.is_resource("sub.py"))
print("__init__.py" in sub_contents)
print("sub.py" in sub_contents)
