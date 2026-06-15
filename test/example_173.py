import import_value

loader = import_value.__loader__
print(loader.get_filename(import_value.__name__).endswith("test/import_value.py"))
loader.path = "changed.py"
print(loader.get_filename(import_value.__name__))
loader.name = "renamed_value"
print(loader.name)
print(loader.get_filename(loader.name))
