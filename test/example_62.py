items = ["a", "b"]
en = enumerate(items)
print(iter(en) is en)
first = next(en)
second = next(en)
print(first, second)
print(first is second)

started = enumerate((10, 20), 5)
print(next(started), next(started))

text = enumerate("xy", True)
print(next(text), next(text))

items.append("c")
late = enumerate(items, -1)
print(next(late), next(late), next(late))

direct = enumerate([9])
print(direct.__next__())
