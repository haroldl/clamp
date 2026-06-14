items = [10, 30]
print(items.insert(1, 20), items)
items.insert(-1, 25)
print(items)
items.insert(-99, 5)
items.insert(99, 35)
print(len(items), items[0], items[1], items[2], items[3], items[4], items[5])
empty = []
empty.insert(-1, 1)
empty.insert(99, 2)
print(empty)
