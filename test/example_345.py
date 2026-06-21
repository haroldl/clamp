import _decimal
import _hashlib
import _zoneinfo

left = _decimal.Decimal('1.25')
right = _decimal.Decimal('2.75')
print(left < right)
print(left <= right)
print(right > left)
print(right >= left)
print(left == _decimal.Decimal('1.25'))
print(left != right)
print(hash(_zoneinfo.ZoneInfo('UTC')) == hash(_zoneinfo.ZoneInfo('UTC')))
h = _hashlib.openssl_sha256(b'abc')
print(len(h.digest()))
print(h.digest() == bytes.fromhex('ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'))
print(h.digest() != b'')
print(_zoneinfo.ZoneInfo('UTC') == _zoneinfo.ZoneInfo('UTC'))
