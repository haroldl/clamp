print(all([True, 1, "x"]), all([True, 0, "x"]))
print(any([False, 0, ""]), any([False, 0, "x"]))
print(all([]), any([]))
print(all((1, True, "a")), any((0, False, "")))
print(all("abc"), any(""))

items = iter([1, 0, 2])
print(all(items))
print(next(items))

letters = iter("ab")
print(any(letters))
print(next(letters))
