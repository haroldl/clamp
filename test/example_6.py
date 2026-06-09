print(True, False, None)
if None:
    print("bad none")
else:
    print("none false")
if []:
    print("bad empty")
else:
    print("empty false")
if [1]:
    print("list true")
print(None or 42)
print(0 or "fallback")
print(5 and "kept")
print([] and "bad")
print([1] and "ok")
print(True == 1, False == 0, None == False, "x" == "x")
if 0:
    print("bad zero")
else:
    print("zero false")
if "":
    print("bad string")
else:
    print("string false")
