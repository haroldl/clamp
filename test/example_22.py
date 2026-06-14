items = [1, 2, 3, 2, None, "x"]
print(items.remove(2), items)
items.remove(2)
print(items)
bools = [1, True, 0, False]
bools.remove(True)
print(bools)
bools.remove(False)
print(bools)
items.remove(None)
items.remove("x")
print(items)
alias = items
print(alias.remove(3), items, alias)
