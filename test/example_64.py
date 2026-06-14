print(min([3, 1, 2]), max([3, 1, 2]))
print(min((3, 1, 2)), max((3, 1, 2)))
print(min("clamp"), max("clamp"))
print(min(3, 1, 2), max(3, 1, 2))

nested = [[2], [1]]
smallest = min(nested)
biggest = max(nested)
nested[0].append(9)
print(smallest, biggest)

print(min(True, 0, 2), max(False, 1, 2))
first = [1]
second = [1]
print(min(first, second) is first, max(first, second) is first)
