import _json
import _decimal

print('encode_basestring_ascii' in dir(_json))
print('__name__' in dir(_json))
module_dict = vars(_json)
print(type(module_dict).__name__)
print('encode_basestring_ascii' in module_dict)
print(module_dict['__name__'])
_json.__clamp_dir_probe__ = 9
print('__clamp_dir_probe__' in dir(_json))
print(vars(_json)['__clamp_dir_probe__'])
del _json.__clamp_dir_probe__
print('__clamp_dir_probe__' in dir(_json))
print('Decimal' in dir(_decimal))
print(vars(_decimal)['__name__'])
