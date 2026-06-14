pass

print("start")

if False:
    print("bad")
else:
    pass
    print("else")

total = 0
for item in [1, 2, 3]:
    if item == 2:
        pass
    total += item
print(total)

def choose(flag):
    if flag:
        pass
        return "yes"
    pass
    return "no"

print(choose(True))
print(choose(False))
