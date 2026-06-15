plain = __import__("import_pkg.sub")
print(plain.__name__)
print(plain.sub.__name__)
print(plain.sub.VALUE)

leaf = __import__("import_pkg.sub", None, None, ["VALUE"])
print(leaf.__name__)
print(leaf.VALUE)

pkg = __import__("import_pkg", None, None, ["sub"])
print(pkg.__name__)
print(pkg.sub.VALUE)

value = __import__("import_value", None, None, ("VALUE",))
print(value.__name__)
print(value.VALUE)

print(__import__("import_value", None, None, []).__name__)
print(__import__("import_value", None, None, None).__name__)
print(__import__("import_value", None, None, ["missing"]).__name__)
print(__import__("import_pkg", None, None, ["missing"]).__name__)
