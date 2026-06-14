items = [1, 2]
alias = items
items *= 3
print(items)
print(alias)

returned = items.__imul__(0)
print(returned)
print(alias)

one = ["x"]
one_alias = one
returned_one = one.__imul__(1)
print(returned_one)
print(one_alias)

nested = [[1]]
nested_alias = nested
nested *= 2
nested[0].append(2)
print(nested_alias)

number = 2
number *= True
print(number)

text = "ha"
text *= 3
print(text)
