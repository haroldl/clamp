from typing import NamedTuple

class Pair(NamedTuple):
    left: object
    right: object

p = Pair(1, 2)
print(p.left)
print(p.right)
