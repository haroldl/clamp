import importlib
import importlib.machinery
import sys

import _json
import _decimal
import _hashlib
import _bz2
import _zoneinfo

mods = [_json, _decimal, _hashlib, _bz2, _zoneinfo]
for mod in mods:
    print(mod.__spec__.name)
    print(type(mod.__spec__.loader).__name__)
    print(mod.__spec__.origin.endswith(tuple(importlib.machinery.EXTENSION_SUFFIXES)))
    print(mod.__loader__ is mod.__spec__.loader)
    print(sys.modules[mod.__spec__.name] is mod)

print(importlib.import_module('_json') is _json)
print(_decimal.Decimal('1.25') + _decimal.Decimal('2.75'))
print(type(_decimal.Decimal('3.5')).__name__)
print(_hashlib.openssl_sha256(b'abc').hexdigest())
comp = _bz2.BZ2Compressor()
blob = comp.compress(b'abc') + comp.flush()
print(_bz2.BZ2Decompressor().decompress(blob))
print(_zoneinfo.ZoneInfo('UTC').key)

try:
    _decimal.Decimal('not-a-number')
except _decimal.InvalidOperation as exc:
    print(type(exc).__name__)
    print(isinstance(exc, _decimal.InvalidOperation))

loader = _decimal.__spec__.loader
print(loader.name)
print(type(loader).__name__)
print(loader.path.endswith(tuple(importlib.machinery.EXTENSION_SUFFIXES)))
