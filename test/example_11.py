arr = [7, 8]
it = arr.__iter__()
print(it)
print(it.__iter__())
print(it.__next__())
print(it.__next__())
print(arr.__iter__().__next__())
