items = [1, "x", None, True, False]
print(items.__repr__())

nested = [["a"], []]
repr_text = nested.__repr__()
nested[0].append("b")
print(repr_text)
print(nested.__repr__())

print([].__repr__())
print([[1, "two"], [False]].__repr__())
