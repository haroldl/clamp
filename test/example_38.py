items = [0, 1, 2, 3, 4, 5, 6]
del items[2:5]
print(items)

front = [0, 1, 2, 3, 4]
del front[:2]
print(front)

tail = [0, 1, 2, 3, 4]
del tail[3:]
print(tail)

stepped = [0, 1, 2, 3, 4, 5, 6]
del stepped[::2]
print(stepped)

reverse_step = [0, 1, 2, 3, 4, 5, 6]
del reverse_step[5:1:-2]
print(reverse_step)

alias = ["a", "b", "c", "d"]
same = alias
del same[1:3]
print(alias, same)

nested = [[1], [2], [3]]
removed = nested[1]
del nested[1:2]
removed.append(4)
print(nested, removed)
