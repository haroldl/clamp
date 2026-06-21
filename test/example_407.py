import _decimal

x = _decimal.Decimal('12.75')
y = _decimal.Decimal('12')
print(type(x).__module__)
print(int(y), type(int(y)).__name__)
print(float(x), type(float(x)).__name__)
print(str(x))
print(bytes([65, 66, 67]))
print(bytes())
print(bytes('hi', 'utf-8'))
