import import_value

spec = import_value.__spec__
suffix = import_value.__cached__[-35:]
print(spec.cached.endswith(suffix))

spec.cached = None
spec.origin = "module.spamspamspam"
print(spec.cached is None)
print(spec._cached is None)

spec.cached = None
spec.origin = "bytecode.pyc"
print(spec.cached)
print(spec._cached)

spec.cached = None
spec.origin = "source.py"
print(spec.cached.endswith("__pycache__/source.cpython-314.pyc"))
print(spec._cached.endswith("__pycache__/source.cpython-314.pyc"))
