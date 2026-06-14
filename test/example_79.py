values = (1, 2)
same = values * 1
print(same is values, same)
zero = values * 0
print(zero is values, zero)
nested = ([1],)
repeated = nested * 2
nested[0].append(2)
print(repeated)
listed = [1, 2]
print((listed * 1) is listed, listed * 1)
bool_repeat = values * True
print(bool_repeat is values, bool_repeat)
