def key(item):
    return item[1]


print(sorted(["aaa", "b", "cc"], key=len))
print(sorted([("a", 2), ("b", 1)], key=key, reverse=True)[0][0])
