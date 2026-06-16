data = {"first": 1, "second": 2, "third": 3}
print(list(data))
print(tuple(data))

seen = []
for key in data:
    seen.append(key)
print(seen)

del data["second"]
data["second"] = "again"
print(list(data))

iterator = iter(data)
print(iterator.__length_hint__())
print(next(iterator), iterator.__length_hint__())
print(next(iterator), iterator.__length_hint__())
print(next(iterator), iterator.__length_hint__())
print(next(iterator, "done"), iterator.__length_hint__())

print(list({}))
