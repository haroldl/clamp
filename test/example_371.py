from dataclasses import dataclass

@dataclass
class Item:
    a: int
    b: int = 2

first = Item(a=1)
print(first.a)
print(first.b)
second = Item(3, b=4)
print(second.a)
print(second.b)
try:
    Item()
except TypeError as exc:
    print(type(exc).__name__)

@dataclass(slots=True)
class SlotLike:
    name: str

slot = SlotLike(name="ok")
print(slot.name)
