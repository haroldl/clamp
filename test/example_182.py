import import_value

spec = import_value.__spec__
loader = import_value.__loader__

eq_loader = spec.__eq__(loader)
ne_loader = spec.__ne__(loader)
eq_none = spec.__eq__(None)
ne_none = spec.__ne__(None)

print(eq_loader is ne_loader)
print(eq_loader is eq_none)
print(eq_loader is ne_none)
print(type(eq_loader).__name__)
print(repr(eq_loader))
print(spec == loader)
print(spec != loader)
print(spec.__eq__(spec))
print(spec.__ne__(spec))
