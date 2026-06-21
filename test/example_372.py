from functools import partial, cached_property

class Demo:
    @classmethod
    def cm(cls):
        return cls.__name__

    @staticmethod
    def sm():
        return "static"

print(Demo.cm())
print(Demo.cm.__func__.__name__)
print(Demo.cm.__func__ is Demo.cm)
print(Demo.sm())
print(Demo.__dict__["sm"].__func__.__name__)

def plain():
    return None

print(isinstance(Demo.__dict__["cm"], classmethod))
print(isinstance(Demo.__dict__["sm"], staticmethod))
print(isinstance(plain, classmethod))
print(isinstance(plain, staticmethod))
print(isinstance(plain, partial))
print(isinstance(plain, cached_property))
