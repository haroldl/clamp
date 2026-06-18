import sys
from . import a
print(sys.modules["circular_rel.a"] is a)
print(a.__spec__._initializing)
print("A" in a.__dict__)
