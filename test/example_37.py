items = [1, True, None, "x", [2]]
print(items.__contains__(1), items.__contains__(False))
print(items.__contains__(None), items.__contains__("x"))
needle = [2]
print(items.__contains__(needle))
print([].__contains__(None))
