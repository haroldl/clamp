import import_value
import import_pkg
import import_pkg.sub

value_source = import_value.__loader__.get_source(import_value.__name__)
print(type(value_source).__name__)
print(value_source.endswith(chr(10)))
print(value_source.splitlines()[0])

pkg_source = import_pkg.__loader__.get_source(import_pkg.__name__)
print(type(pkg_source).__name__)
print(pkg_source.endswith(chr(10)))
print(pkg_source.splitlines()[0])

sub_source = import_pkg.sub.__loader__.get_source(import_pkg.sub.__name__)
print(type(sub_source).__name__)
print(sub_source.endswith(chr(10)))
print(sub_source.splitlines()[0])
