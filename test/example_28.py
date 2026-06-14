items = [0, 1, 2, 3, 4]
print(items[1:4])
print(items[:3], items[3:])
print(items[-4:-1])
print(items[::2], items[1::2])
print(items[::-1], items[3:0:-1])
print(items[99:100], items[-99:2])
copy = items[:]
items[1] = 99
print(copy, items)
nested = [["x"], ["y"]]
part = nested[:1]
nested[0].append("z")
print(part, nested)
