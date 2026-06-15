pkg = __import__("import_pkg", None, None, "sub")
print(pkg.__name__)

leaf = __import__("import_pkg.sub", None, None, "VALUE")
print(leaf.__name__)

nonpackage_leaf = __import__("import_pkg.sub", None, None, 1)
print(nonpackage_leaf.__name__)

root = __import__("import_pkg.sub", None, None, False)
print(root.__name__)

value = __import__("import_value", None, None, 1)
print(value.__name__)
