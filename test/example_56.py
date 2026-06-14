items = ([], 1, "x")
args = items.__getnewargs__()

print(args)
print(len(args), args[0] is items, args[0][0] is items[0])

args[0][0].append(2)
print(items)
print(args)

empty_args = ().__getnewargs__()
print(empty_args, len(empty_args), len(empty_args[0]))
