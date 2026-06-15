import import_value

loader = import_value.__loader__
loader.name = "renamed_value"
print(loader.get_filename(import_value.__name__))
