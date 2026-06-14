items = [10, 20, 30, 40]
print(items.pop(), len(items), items)
print(items.pop(1), len(items), items[0], items[1])
items.append(50)
print(items.pop(-2), len(items), items)
nested = [[1, 2], [3]]
print(nested.pop(0), len(nested), nested[0])
