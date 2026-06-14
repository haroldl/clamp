text = "clamp"
print(text.__len__(), len(text))
print(text.__getitem__(0), text.__getitem__(-1))
print(text.__getitem__(slice(1, 4)), text[1:4])
print(text.__getitem__(slice(None, None, -1)))
print("".__len__(), "x".__getitem__(0))
