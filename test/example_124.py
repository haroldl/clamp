items = []
items.append(items)
print(items)
print(repr(items))
outer = [items]
print(outer)
cycle = []
wrapper = (cycle,)
cycle.append(wrapper)
print(wrapper)
print(cycle)
regular = [1, [2, 3], ("x",)]
print(regular)
