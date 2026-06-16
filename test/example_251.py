import operator

items = [1, 2, 3]
it = iter(items)
print(operator.length_hint(items))
print(operator.length_hint(it))
print(next(it))
print(operator.length_hint(it))
print(operator.length_hint(iter(()), 7))
print(operator.length_hint(reversed("abc")))
print(operator.length_hint(iter({"a": 1, "b": 2})))
