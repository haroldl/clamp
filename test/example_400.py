from functools import wraps
import inspect


def original():
    return "original"


@wraps(original)
def wrapper():
    return "wrapper"


@wraps(wrapper)
def outer():
    return "outer"


print(inspect.unwrap(outer) is original)
print(inspect.unwrap(outer, stop=lambda fn: fn is wrapper) is wrapper)
print(outer())
