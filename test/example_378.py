from typing import Generic, TypeVar
T = TypeVar("T")
class Plain:
    pass
class Box(Plain, Generic[T]):
    value: T
print(Box.__name__)
print(issubclass(Box, Plain))
