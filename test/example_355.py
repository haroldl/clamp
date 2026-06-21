import _decimal

value = _decimal.Decimal("12.345")
print(format(value, ".2f"))
print(f"{value:.2f}")
print(f"{value:>8.1f}")
print(f"{value!s:>8}")
print(f"{value!r}")
