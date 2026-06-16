import import_value

reader = import_value.__loader__.get_resource_reader(import_value.__name__)
handle = reader.open_resource("import_value.py")

print(type(handle).__name__)
print(handle.isatty())
print(handle.tell())
print(handle.read(5))
print(handle.tell())
print(handle.isatty())
handle.seek(0, 2)
print(handle.isatty())
print(handle.tell() > 0)
handle.close()
print(handle.closed)
