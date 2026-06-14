values = [0, 1, 2, 3, 4, 7, 8, 37, -1, -2, -37]
for value in values:
    print(value, value.bit_length())

print(True.bit_length(), False.bit_length())
