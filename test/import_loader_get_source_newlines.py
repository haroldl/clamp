import import_value

loader = import_value.__loader__
loader.path = "/tmp/clamp_import_loader_crlf_source.py"
source = loader.get_source(import_value.__name__)
print(source == "left\nright\n")
print(source.count(chr(13)))
print(source.splitlines())
