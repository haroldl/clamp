left = [None]
alias = left
copy = left.copy()
nested = [left]
same_nested = nested[0]

print(left is alias, left is copy, left is not copy)
print(None is None, True is True, False is not True)
print(same_nested is left, same_nested is copy)
print(left is alias is same_nested)

left.append(1)
print(alias is left, copy)

def check(value):
    if value is None:
        print("none")
    if value is not None:
        print("value")

check(None)
check(left)
