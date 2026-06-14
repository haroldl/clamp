left = [1, True, None, "x"]
same = [1, 1, None, "x"]
different_value = [1, True, None, "y"]
shorter = [1, True, None]

print(left.__eq__(same))
print(left.__ne__(same))
print(left.__eq__(different_value), left.__ne__(different_value))
print(left.__eq__(shorter), left.__ne__(shorter))

nested_left = [[1], ["a"]]
nested_same = [[True], ["a"]]
nested_different = [[1], ["b"]]
print(nested_left.__eq__(nested_same), nested_left.__ne__(nested_same))
print(nested_left.__eq__(nested_different), nested_left.__ne__(nested_different))

alias = left
left.append(2)
print(alias.__eq__(left), alias.__ne__(left))
