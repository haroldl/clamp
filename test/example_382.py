def outer():
    def inner():
        yield 1
    return tuple(inner())

print(outer())
print(type(outer()).__name__)
