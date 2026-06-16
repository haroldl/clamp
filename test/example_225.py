import import_value

reader = import_value.__loader__.get_resource_reader(import_value.__name__)
handle = reader.open_resource("import_value.py")
iterator = iter(handle)
print(iterator is handle)

first = next(iterator)
print(type(first).__name__)
print(first)
print(handle.tell())
print(next(iterator, "done"))
print(next(iterator, "done"))
print(handle.tell())

handle.seek(0)
lines = []
for line in handle:
    lines.append(line)
print(len(lines), lines[0], handle.tell())
print(next(handle, "done"))

handle.seek(0)
print(handle.__iter__() is handle)
print(handle.__next__())
print(next(handle, "done"))
print(next(handle, "done"))
