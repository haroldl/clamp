items = [1, [2]]
print(items.__add__([3]))
print(items.__mul__(2))
print(items.__rmul__(2))
print(items.__add__([]))

nested = [[1]]
repeated = nested.__mul__(2)
nested[0].append(2)
print(repeated)

left = [0]
combined = left.__add__(nested)
left.append(9)
nested[0].append(3)
print(combined)
