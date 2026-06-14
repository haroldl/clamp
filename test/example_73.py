print(list(), tuple(), len(list()), len(tuple()))

items = [1, [2]]
copied = list(items)
items.append(3)
copied[1].append(4)
print(items, copied, items is copied, items[1] is copied[1])

print(list("ab"), tuple("xy"))
print(list((1, 2)), tuple([3, 4]))

same_tuple = (5, [6])
converted = tuple(same_tuple)
same_tuple[1].append(7)
print(converted is same_tuple, converted)

it = iter([8, 9])
print(tuple(it), list(it))
