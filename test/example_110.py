text = "banana bandana"
print(text.__contains__("nan"))
print(text.__contains__("x"))
print(text.__contains__(""))
print("".__contains__(""))
print("abc".__contains__(chr(98)))
print("nan" in text, text.__contains__("nan"))
print(["abc".__contains__("a"), "abc".__contains__("z")])
