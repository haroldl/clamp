left = [1, [2]]
right = [3]
joined = left + right
print(joined)
print(left, right)
left.append(4)
print(joined, left)
joined[1].append("x")
print(left, joined)
print([] + [], [1] + [], [] + [2])
print(1 + True, "py" + "thon")
