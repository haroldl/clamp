items = [1, 2, 3]
alias = items
print(items.clear(), len(items), items, alias)
items.append(4)
print(len(alias), alias[0], items)
empty = []
print(empty.clear(), len(empty), empty)
