text = "clamp"
it = iter(text)
print(it.__length_hint__())
print(next(it), it.__length_hint__())
again = iter(it)
print(next(again), next(it), it.__length_hint__())
print(next(it), next(it), it.__length_hint__())

empty = iter("")
print(empty.__length_hint__())

solo = iter("x")
print(next(solo), solo.__length_hint__())
