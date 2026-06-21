print(list(zip([1], [2], strict=True)))
try:
    list(zip([1, 2], [3], strict=True))
except ValueError as exc:
    print(type(exc).__name__)
