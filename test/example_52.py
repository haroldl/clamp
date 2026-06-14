items = (1, 2, 3)
it = reversed(items)
print(it.__length_hint__())
print(next(it), it.__length_hint__())
print(next(it), next(it), it.__length_hint__())

fresh = reversed((10,))
print(next(fresh), fresh.__length_hint__())

nested = ([1], [2])
rit = reversed(nested)
first = next(rit)
first.append(99)
print(first, nested)

empty = reversed(())
print(empty.__length_hint__())
