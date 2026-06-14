left = [1, 2]
right = [1, 3]
same = [1, 2]
prefix = [1]
print(left.__lt__(right), left.__le__(same), right.__gt__(left), prefix.__ge__(left))
print([1, [2]].__lt__([1, [3]]), [1, True].__ge__([1, 1]))

tuple_left = (1, 2)
tuple_right = (1, 3)
tuple_same = (1, 2)
tuple_prefix = (1,)
print(tuple_left.__lt__(tuple_right), tuple_left.__le__(tuple_same), tuple_right.__gt__(tuple_left), tuple_prefix.__ge__(tuple_left))
print((1, (2,)).__lt__((1, (3,))), (1, True).__ge__((1, 1)))
