items = [1, 2]
returned = items.extend([3, 4])
print(items)
print(returned)
items.extend([])
print(items, len(items))
items.extend(items)
print(items)
nested = [["x"]]
copy = []
copy.extend(nested)
nested[0].append("y")
print(copy)
copy[0].append("z")
print(nested)
