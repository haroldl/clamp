from functools import wraps


def original(value: int) -> int:
    return value


@wraps(original)
def wrapper(value):
    return original(value)


print(wrapper.__name__, wrapper.__qualname__)
print(wrapper.__wrapped__ is original)
print(wrapper.__annotations__["value"] is int, wrapper.__annotations__["return"] is int)
