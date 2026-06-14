items = []
print(len(items), len([1, 2, 3]), len("clamp"), len(""))
items.append("x")
items.append("y")
print(len(items), items[0], items[1])
nested = [[], [1]]
print(len(nested), len(nested[0]), len(nested[1]))
if len([]):
    print("bad len false")
else:
    print("empty len false")
if len([0]):
    print("nonempty len true")
