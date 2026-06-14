items = [1, [2], "x"]
copied = items.copy()
print(items, copied)
items.append(3)
print(items, copied)
copied[1].append(4)
print(items, copied)
empty = []
print(empty.copy())
