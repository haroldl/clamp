import import_value

data = import_value.__loader__.get_data(import_value.__file__)
head = data[:5]
it = head.__iter__()

print(list(head))
print(type(it).__name__)
print(it.__iter__() is it)
print(it.__length_hint__())
print(next(it))
print(it.__length_hint__())
print(it.__next__())
print(list(it))
print(list(data[0:0]))
print(sum(data[:5]))
