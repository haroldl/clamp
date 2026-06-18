import import_value

spec = import_value.__spec__
print(import_value.__cached__ is None)
print(spec.cached is None)

spec.cached = None
spec.origin = "module.spamspamspam"
print(spec.cached is None)
print(spec._cached is None)

spec.cached = None
spec.origin = "bytecode.pyc"
print(spec.cached is None)
print(spec._cached is None)

spec.cached = None
spec.origin = "source.py"
print(spec.cached is None)
print(spec._cached is None)
