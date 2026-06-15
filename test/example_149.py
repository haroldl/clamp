print("py".__add__("thon"))
print("ha".__mul__(3), "ha".__rmul__(2))
print("x".__mul__(0), "x".__rmul__(-2))

source = "ab"
combined = source.__add__("cd")
print(combined, source)
print("go".__mul__(True), "go".__mul__(False))
print(["na".__mul__(2), "ba".__rmul__(2)])
