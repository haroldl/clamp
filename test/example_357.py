import _decimal

value = _decimal.Decimal("12.345")
print(round(value))
print(round(value, 1))
print(round(value, -1))
print(type(round(value, 1)).__name__)
