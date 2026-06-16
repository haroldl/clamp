import import_value

reader = import_value.__loader__.get_resource_reader(import_value.__name__)
handle = reader.open_resource("import_value.py")

entered = handle.__enter__()
print(entered is handle)
print(handle.closed)
print(len(entered.read(5)))
print(handle.__exit__(None, None, None))
print(handle.closed)
