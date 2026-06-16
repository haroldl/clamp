import import_pkg

reader = import_pkg.__loader__.get_resource_reader(import_pkg.__name__)
files = reader.files()
init_path = files.joinpath("__init__.py")
sub_path = files.joinpath("sub.py")
init_data = init_path.read_bytes()
sub_data = sub_path.read_bytes()
opened_data = sub_path.open("rb").read()
print(type(init_data).__name__)
print(init_data)
print(sub_data[:5])
print(opened_data[:5])
