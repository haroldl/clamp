i = 0
while i < 5:
    i += 1
    if i == 2:
        continue
    if i == 4:
        break
    print("loop", i)
else:
    print("bad break else")
print("after break", i)

j = 0
while j < 2:
    print("plain", j)
    j += 1
else:
    print("plain else", j)

outer = 0
while outer < 2:
    inner = 0
    while inner < 3:
        inner += 1
        if inner == 2:
            break
        print("nested", outer, inner)
    outer += 1
print("nested done")
