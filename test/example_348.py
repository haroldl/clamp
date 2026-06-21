import _json

_json.__clamp_probe__ = 'set'
print(_json.__clamp_probe__)
print(hasattr(_json, '__clamp_probe__'))
delattr(_json, '__clamp_probe__')
print(hasattr(_json, '__clamp_probe__'))
setattr(_json, '__clamp_probe2__', 42)
print(getattr(_json, '__clamp_probe2__'))
del _json.__clamp_probe2__
print(hasattr(_json, '__clamp_probe2__'))
