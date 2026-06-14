values = [0, 1, False, True, "", "x", [], [2], None, (3,)]
truthy = filter(None, values)
print(next(truthy))
print(next(truthy))
print(next(truthy))
print(next(truthy))

letters = filter(bool, "a b")
print(next(letters))
print(next(letters))
print(next(letters))

source = [0]
lazy = filter(None, source)
source.append(4)
print(next(lazy))

for item in filter(None, (0, "ok", (), (5,), False)):
    print(item)
