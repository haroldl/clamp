source = [3, 1, 2]
ordered = sorted(source)
print(ordered)
print(source)

print(sorted(("b", "a", "c")))
print(sorted("cab"))
print(sorted([]))

values = iter([2, 1, 3])
print(sorted(values))
print(values.__length_hint__())

nested = [[2], [1]]
copied = sorted(nested)
nested[0].append(0)
print(copied)
print(nested)
