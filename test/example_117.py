text = "banana bandana"

print(text.startswith(("ban", "can")))
print(text.startswith(("can", "ban")))
print(text.startswith(("ana", "ban"), 1))
print(text.startswith((), 0))
print(text.endswith(("ana", "band")))
print(text.endswith(("band", "ana")))
print(text.endswith(("ban", "band"), 0, -3))
print(text.endswith((), 0))
