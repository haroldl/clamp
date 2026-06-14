def add(left, right):
    return left + right

values = [1, 2]
mapped = map(str, values)
values.append(3)
print(next(mapped))
print(list(mapped))

print(list(map(bool, [0, 1, "", "x", [], [5], None])))
print(list(map(add, [1, 2, 3], (10, 20))))
print(list(map(tuple, ["ab", "", [1, 2]])))

again = map(str, range(2))
print(iter(again) is again)
print(again.__next__())
print(next(again))
