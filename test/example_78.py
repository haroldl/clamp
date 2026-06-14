items = [0, 1, 2, 3, 4]
items[1:4] = [7, 8]
print(items)
items[:0] = [-2, -1]
print(items)
items[3:3] = "ab"
print(items)
items[::2] = [20, 21, 22, 23]
print(items)
