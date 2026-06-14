items = [1, 2, 3]
print(items.__len__(), len(items))
items.append(4)
print(items.__len__(), len(items))
items.pop()
print(items.__len__(), len(items))
empty = []
print(empty.__len__(), len(empty))
alias = items
alias.clear()
print(items.__len__(), alias.__len__(), len(items))
