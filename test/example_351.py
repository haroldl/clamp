import _json
import _decimal

enc = _json.encode_basestring_ascii
decimal_type = _decimal.Decimal
value = _decimal.Decimal("12.30")
print(repr(_json).startswith("<module '_json'"))
print("encode_basestring_ascii" in repr(enc))
print(repr(decimal_type).startswith("<class 'decimal.Decimal'"))
print(repr(value))
print(f"{value!r}")
print(str(value))
print(repr(value) == str(value))
