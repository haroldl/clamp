import _decimal
import importlib

Decimal1 = _decimal.Decimal
Decimal2 = _decimal.Decimal
value = Decimal1("1.2")
print(Decimal1 is Decimal2)
print(type(value) is Decimal1)
print(type(value) is _decimal.Decimal)
print(_decimal is importlib.import_module("_decimal"))
print(_decimal.__spec__.loader is _decimal.__loader__)
print(_decimal.__spec__ is _decimal.__spec__)
