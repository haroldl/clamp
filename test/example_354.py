import _decimal

print(issubclass(_decimal.Decimal, _decimal.Decimal))
print(issubclass(_decimal.Decimal, object))
print(issubclass(_decimal.Decimal, (_decimal.Decimal, str)))
print(issubclass(_decimal.Context, _decimal.Context))
print(issubclass(_decimal.Context, object))
print(issubclass(_decimal.Context, (_decimal.Decimal, _decimal.Context)))
