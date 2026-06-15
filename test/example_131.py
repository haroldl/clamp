xs = []
print(xs.__sizeof__())
xs.append(1)
print(xs.__sizeof__())
xs.append(2)
xs.append(3)
xs.append(4)
xs.append(5)
print(xs.__sizeof__())
ys = [1]
print(ys.__sizeof__())
zs = [1, 2, 3]
print(zs.__sizeof__())
zs.insert(1, 9)
print(zs.__sizeof__())
