import sys
try:
    from .a import A
except ImportError:
    print("ImportError")
    print("circular_rel.a" in sys.modules)
    print("A" in sys.modules["circular_rel.a"].__dict__)
from . import a
print(sys.modules["circular_rel.a"] is a)
print(a.__spec__._initializing)
print("A" in a.__dict__)
