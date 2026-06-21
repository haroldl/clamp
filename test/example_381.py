from typing import Generic, TypeVar

T = TypeVar("T")

class Box(Generic[T]):
    pass

print(Box.__parameters__[0].__name__)
print("Generic" in str(Box.__orig_bases__[0]))
