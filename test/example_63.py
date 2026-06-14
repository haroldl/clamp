pairs = zip([1, 2, 3], ("a", "b"))
print(iter(pairs) is pairs)
first = next(pairs)
second = next(pairs)
print(first, second)
print(first is second)

text_pairs = zip("xy", [10, 20])
print(next(text_pairs), next(text_pairs))

nested = [[1], [2]]
zipped_nested = zip(nested, ("left", "right"))
nested[0].append(9)
print(next(zipped_nested))

empty = zip()
print(iter(empty) is empty)

short = zip([1], "abc")
print(next(short))
