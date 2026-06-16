import import_pkg

reader = import_pkg.__loader__.get_resource_reader(import_pkg.__name__)
files = reader.files()
init_path = files.joinpath("__init__.py")
sub_path = files.joinpath("sub.py")
init_text = init_path.read_text()
sub_text = sub_path.read_text("utf-8")
print(type(init_text).__name__)
print(init_text)
print(sub_text.splitlines()[0])
print(init_path.read_text("utf-8") == init_text)
