a = []
b = a
c = []
print(id(a) == id(a) + 0)
print(id(a) == id(b))
print(id(a) == id(c))
print(id(None) == id(None))
print(id(True) == id(True))
print(id(False) == id(False))
s = "clamp"
t = s
print(id(s) == id(t))
items = [a, c]
print(id(items[0]) == id(a))
print(id(items[1]) == id(c))
