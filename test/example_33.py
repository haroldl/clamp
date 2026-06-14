items = [1, 2, 3]
it = reversed(items)
items.append(4)
print(next(it), next(it), next(it), items)
print(next(reversed(items)), items)
nested = [["x"], ["y"]]
rit = reversed(nested)
first = next(rit)
first.append("z")
print(first, nested)
print(next(rit))
