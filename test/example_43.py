print(bool())
print(bool(None), bool(False), bool(True))
print(bool(0), bool(1), bool(-1))
print(bool(""), bool("clamp"))
print(bool([]), bool([0]))

items = []
print(bool(items))
items.append(None)
print(bool(items))
items.clear()
print(bool(items))
