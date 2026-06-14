items = [1, 2, 3]
it = iter(items)
print(it.__length_hint__())
print(next(it), it.__length_hint__())
items.append(4)
print(it.__length_hint__())
print(next(it), next(it), next(it), it.__length_hint__())

ritems = [1, 2, 3]
rit = reversed(ritems)
print(rit.__length_hint__())
print(next(rit), rit.__length_hint__())
ritems.pop()
print(rit.__length_hint__())
ritems.pop()
print(rit.__length_hint__())

empty = iter([])
print(empty.__length_hint__())
