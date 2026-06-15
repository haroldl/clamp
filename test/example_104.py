items = iter([1])
print(next(items, "empty"))
print(next(items, "empty"))
print(next(items, "again"))

letters = iter("ab")
print(next(letters, None), next(letters, None), next(letters, None) is None)

zipped = zip([1], [2, 3])
print(next(zipped, "done"))
print(next(zipped, "done"))

mapped = map(str, [])
print(next(mapped, "fallback"))

filtered = filter(None, [0, "", False])
print(next(filtered, "fallback"))

default = []
empty = iter(())
print(next(empty, default) is default)
