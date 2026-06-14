print(repr(None), repr(True), repr(False))
print(repr(0), repr(3.5), repr("clamp"))

items = [1, "two", False]
text = repr(items)
print(text)
items.append([None])
print(text)
print(repr(items))
print(repr([[1, "two"], [False]]))
