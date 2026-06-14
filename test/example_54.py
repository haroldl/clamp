items = [1]
result = items.extend((2, 3))
print(result, items)

items += (4, 5)
print(items)

self_items = [7, 8]
self_items.extend(self_items)
print(self_items)
