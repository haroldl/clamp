items = [1]
result = items.extend("ab")
print(items, result)

items += "cd"
print(items)

empty = []
empty.extend("")
print(empty)

nested = [[]]
nested.extend("x")
nested[0].append(9)
print(nested)
