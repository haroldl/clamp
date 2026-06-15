text = "ab"
it = text.__iter__()
print(it.__iter__() is it)
print(it.__length_hint__())
print(it.__next__(), it.__length_hint__())
print(it.__next__(), it.__length_hint__())
print(list(text.__iter__()))
