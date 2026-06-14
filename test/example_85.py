print(str(), str("clamp"), str(repr("clamp")))
print(str(None), str(True), str(False))
print(str(0), str(-2), str(3.5))
print(str([1, "two", False]), str((1, "two")), str(range(2, 8, 3)))

items = [1, "two"]
text = str(items)
items.append([None])
print(text)
print(str(items))
