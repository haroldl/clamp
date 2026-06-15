pkg = __import__("import_all_pkg", None, None, ["*"])

print(pkg.__name__)
print(pkg.NAME)
print(pkg.sub.VALUE)
print(pkg.sub.__name__)
