print(not None, not False, not True, not 0, not 1, not [], not [1], not "", not "x")
print(1 != 2, 1 != 1, True != False, None != False)
print(1 < 2, 2 <= 2, 3 > 2, 3 >= 4)
print("a" < "b", "b" <= "b", "c" > "b", "c" >= "d")
print(True == 1, False == 0, True < 2, False <= 0)
print(1 < 2 < 3, 1 < 2 > 3, 3 < 2 < None)
if not []:
    print("not empty-list truth path")
else:
    print("bad")
