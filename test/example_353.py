import _decimal

a = _decimal.Decimal("10.5")
b = _decimal.Decimal("2")
print(a // b)
print(a % b)
print(divmod(a, b))
print(b ** 3)
c = _decimal.Decimal("4")
c += b
print(c)
c -= b
print(c)
c *= b
print(c)
c /= b
print(c)
