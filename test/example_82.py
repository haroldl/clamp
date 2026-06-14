items = [0, 1, 2, 3, 4, 5]
print(items[slice(1, 5, 2)])
print((0, 1, 2, 3, 4, 5)[slice(None, None, -2)])
print("abcdef"[slice(1, None, 2)])
print(range(10)[slice(2, 8, 3)], list(range(10)[slice(2, 8, 3)]))
print(items[slice(3)], "abcdef"[slice(3)])
