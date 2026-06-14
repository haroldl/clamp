items = [1]
alias = items
items += [2]
print(items)
print(alias)

returned = items.__iadd__([3])
print(returned)
print(alias)

same = [4, 5]
same_alias = same
same += same
print(same)
print(same_alias)

nested = [[1]]
nested_alias = nested
nested += [[2]]
nested[0].append(9)
print(nested_alias)

number = 1
number += True
print(number)

text = "a"
text += "b"
print(text)
