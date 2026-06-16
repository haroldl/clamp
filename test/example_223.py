import import_value

reader = import_value.__loader__.get_resource_reader(import_value.__name__)
handle = reader.open_resource("import_value.py")

print(type(handle).__name__)
print(handle.flush())
print(handle.tell())
print(handle.read(5))
print(handle.flush())
print(handle.tell())
handle.close()
print(handle.closed)
