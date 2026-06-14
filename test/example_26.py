items = [1, [2]]
repeated = items * 3
print(repeated)
print(items)
items.append(4)
print(repeated, items)
repeated[1].append("x")
print(items, repeated)
print([] * 5, [1] * 0, [1] * -2)
print(2 * ["a"], ["b"] * True)
print(6 * 7, True * 5, "ha" * 3)
