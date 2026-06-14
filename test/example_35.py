items = [10, 20, 30, 40]
del items[1]
print(items, len(items))
del items[-1]
print(items)
alias = items
del alias[0]
print(items, alias)
nested = [["a"], ["b"], ["c"]]
removed = nested[1]
del nested[1]
removed.append("z")
print(nested, removed)
print(nested.__delitem__(0), nested)
