import import_value

loader = import_value.__loader__
print(loader.__init__("renamed_value", "changed.py"))
print(loader.name)
print(loader.path)
print(loader.get_filename("renamed_value"))
print(loader.get_filename(loader.name))
