i = 0
while i < 3:
    print(i)
    i += 1
print("done", i)

while False:
    print("bad false")
else:
    print("while else ran")

arr = [1]
while arr:
    print("list truthy", arr[0])
    arr = []
print("list done")
