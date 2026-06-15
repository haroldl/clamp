print(bool(()), bool((0,)), len(()), len((0, 1)))
print(bool([]), bool([None]), len([]), len([None, False]))
print(bool(""), bool("abc"), len("abc"))
print(bool(range(0)), bool(range(2)), len(range(0)), len(range(2, 8, 3)))
items = [1]
alias = items
items.clear()
print(bool(alias), len(alias))
items.append("x")
print(bool(alias), len(alias))
