args = (1, 2)
args = (value for value in args)
print(tuple(args))

items = ["a", "b"]
gen = (item.upper() for item in items)
items = ["c"]
print(tuple(gen))
