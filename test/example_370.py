class Plain:
    def __init__(self):
        self.initialized = True

obj = Plain.__new__(Plain)
print(type(obj).__name__)
print(hasattr(obj, "initialized"))
obj.initialized = "manual"
print(obj.initialized)

class Child(Plain):
    pass

child = Child.__new__(Child)
print(type(child).__name__)
print(hasattr(child, "initialized"))

try:
    Plain.__new__(1)
except TypeError as exc:
    print(type(exc).__name__)
