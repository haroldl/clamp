import _collections
import _csv
import _datetime
import _json
import _operator
import _sqlite3
import importlib.machinery
import importlib.util
from _json import encode_basestring_ascii as enc2
from typing import Literal, Tuple

print(_json.__name__)
print(callable(_json))
print(callable(_json.encode_basestring_ascii))
result = _operator.add(1, 2)
print(result)
print(result + 4)
print(type(result).__name__)
encoded = _json.encode_basestring_ascii('a\nb')
print(encoded)
print(type(encoded).__name__)
print(enc2('z'))
print(_operator.getitem([10, 20], 1))
print(_operator.getitem({'x': 5}, 'x') + 1)
combined_list = _operator.concat([1], [2])
print(combined_list)
print(type(combined_list).__name__)
combined_tuple = _operator.concat((1,), (2,))
print(combined_tuple)
print(type(combined_tuple).__name__)

class NativeBase:
    kind = 'base'

class NativeChild(NativeBase):
    value: 'int'

Alias: object = Literal['x'] | None
Repeated = Tuple[Literal['x'], ...]
print(NativeChild.kind)
print(NativeChild.__annotations__['value'])
print(type(Alias).__name__)
print(type(Repeated).__name__)
reader = _csv.reader(['a,b'])
for row in reader:
    print(row)
    print(type(row).__name__)
    print(row[1])
native_dict = _collections.defaultdict()
native_dict['x'] = 3
print(native_dict['x'] + 4)
native_lists = _collections.defaultdict(list)
native_lists['items'].append(1)
native_lists['items'].append(2)
print(native_lists['items'])
print(list(native_lists.items()))
print(native_lists['items'][0:1])
native_lists['items'][0:1] = [9, 8]
print(native_lists['items'])
del native_lists['items'][1:2]
print(native_lists['items'])
print(list(reversed(native_lists['items'])))
native_iter = iter(native_lists['items'])
print(_operator.length_hint(native_iter))
print(next(native_iter), _operator.length_hint(native_iter))
with _sqlite3.connect(':memory:') as native_connection:
    print(native_connection.execute('select 7').fetchone()[0])
del native_dict['x']
print('native item done')
native_date = _datetime.datetime(2020, 1, 1, fold=1)
print(native_date.fold)
print(type(native_date).__name__)
print(importlib.machinery.ExtensionFileLoader.__name__)
print('.so' in importlib.machinery.EXTENSION_SUFFIXES)
spec = importlib.util.find_spec('_json')
print(spec.name)
print(type(spec.loader).__name__)
print(spec.origin.endswith('.so'))
print(_json.__spec__.name)
print(type(_json.__spec__.loader).__name__)
if _operator.not_(True):
    print('bad truth')
else:
    print('native false')
try:
    _operator.add(1)
except TypeError as exc:
    print(type(exc).__name__)
    print('expected 2' in str(exc))
try:
    raise _csv.Error('csv failed')
except _csv.Error as exc:
    print(type(exc).__name__)
    print('csv failed' in str(exc))

from collections.abc import Callable
import sys
sys.path.insert(0, "test")
print(__import__("import_pkg.sub", fromlist=["VALUE"]).__name__)
print(__import__("import_pkg.sub", fromlist=["VALUE"]).VALUE)
from extension_helper_343 import BranchAlias
SetAlias = set[int] | None
FrozenAlias = frozenset[str]
print(Callable.__name__)
print(callable(Callable))
print(callable(callable))
print(type(SetAlias).__name__)
print(type(FrozenAlias).__name__)
print(type(set([1, 2])).__name__)
print(type(BranchAlias).__name__)
print(type(1).__name__)


def CamelName():
    return 'camel'
print(CamelName.__name__)
print(CamelName())

class Outer:
    class Inner:
        label = 'inner'
    def method(self):
        return 'method'
Outer.method.marker = 'marked'
print(Outer.Inner.__name__)
print(Outer.Inner.label)
print(Outer.method.__name__)
print(Outer.method.marker)
print(Outer().method())

head = [1, 2]
tail = (3, 4)
print([0, *head, *tail])
print((*head, *tail, 5))
print(3 in {0, *head, *tail})

d = {}
left = d['key'] = 9
print(left)
print(d['key'])

if (named := 11) > 10:
    print(named)

def numbers():
    yield from [1, 2]
    yield 3
for n in numbers():
    print(n)

from typing import TypeVar
class BoundBase:
    pass
BoundT = TypeVar('BoundT', bound=BoundBase)
print(BoundT.__name__)


class MetaCheck(type):
    def __new__(mcls, name, bases, namespace, **kwargs):
        cls = type.__new__(mcls, name, bases, namespace)
        cls.flag = kwargs.get('flag', 'missing')
        return cls

class MetaMade(metaclass=MetaCheck, flag='seen'):
    marker = 12

print(MetaMade.__name__)
print(MetaMade.marker)
print(MetaMade.flag)
print(issubclass(MetaMade, MetaMade))

class StaticDemo:
    @staticmethod
    def plus_one(value):
        return value + 1

print(StaticDemo.plus_one(4))
print(StaticDemo().plus_one(5))

Built = type.__new__(type, 'Built', (), {'answer': 42})
print(Built.__name__)
print(Built.answer)


def annotated_local_branch(flag):
    raw: object
    if flag:
        raw = {'ok': 1}
    else:
        raw = {}
    return raw.get('ok', 0)

print(annotated_local_branch(True))
print(annotated_local_branch(False))

left_keys = {'a': 1, 'b': 2}.keys()
right_keys = ['b', 'c']
intersection = left_keys & right_keys
print(bool(intersection))
for key in intersection:
    print(key)


import inspect
from contextlib import contextmanager
from dataclasses import field
from functools import cached_property
from typing import NamedTuple, cast, final


def signature_target(a, *, b=1, **extra):
    pass

params = inspect.signature(signature_target).parameters
print('extra' in params)
print(params['extra'].kind == inspect.Parameter.VAR_KEYWORD)

@final
class FinalDemo:
    pass

print(FinalDemo.__name__)
print(cast('type[FinalDemo]', FinalDemo).__name__)

class PropertyDemo:
    @property
    def value(self):
        return 3

    @cached_property
    def cached(self):
        return 4

prop_demo = PropertyDemo()
print(prop_demo.value)
print(prop_demo.cached)
print('__dict__' in dir(prop_demo) or hasattr(prop_demo, '__dict__'))

class Pair(NamedTuple):
    left: object
    right: object

pair = Pair(1, 2)
a, b = pair
print(a + b)
print(pair.left)

field_default = field(default_factory=dict)
print(type(field_default).__name__)
print(field_default == {})

@contextmanager
def managed():
    yield 'managed'

with managed() as value:
    print(value)

class DeleteDemo:
    pass

delete_demo = DeleteDemo()
delete_demo.x = 8
print(hasattr(delete_demo, 'x'))
delattr(delete_demo, 'x')
print(hasattr(delete_demo, 'x'))

class MroBase:
    pass

class MroChild(MroBase):
    pass

print(MroChild.__bases__[0].__name__)
print(MroChild.__mro__[1].__name__)

import sys as _frame_sys

def frame_outer():
    return frame_inner()

def frame_inner():
    f0 = _frame_sys._getframe(0)
    f1 = _frame_sys._getframe(1)
    print(f0.f_code.co_name)
    print(f1.f_code.co_name)
    print(f0.f_back.f_code.co_name)

frame_outer()

def docstring_local(flag):
    "docstring first"
    if flag:
        value = 10
    else:
        value = 20
    return value

print(docstring_local(True))
print(docstring_local(False))

class CallableFuncDemo:
    @classmethod
    def method(cls):
        return 1

print(CallableFuncDemo.method.__func__.__name__)


import importlib.metadata as importlib_metadata
from importlib.metadata import version as metadata_version
print(importlib_metadata.__name__)
print(type(importlib_metadata.distributions()).__name__)
metadata_value = metadata_version('pydantic')
print(type(metadata_value).__name__, len(metadata_value) > 0)
metadata_dist = importlib_metadata.distribution('pydantic')
print(type(metadata_dist).__name__)
print(bool(metadata_dist.metadata['Name']))
try:
    importlib_metadata.version('_definitely_missing_clamp_package_')
except importlib_metadata.PackageNotFoundError as exc:
    print(type(exc).__name__)
print(type(importlib_metadata.entry_points()).__name__)

class MethodLocalDemo:
    def combine(self, obj):
        obj = obj + 1
        extra = 2
        return obj + extra

print(MethodLocalDemo().combine(3))

def plain_plus(value):
    return value + 1

class InstanceCallableDemo:
    pass

instance_callable = InstanceCallableDemo()
instance_callable.fn = plain_plus
print(instance_callable.fn(2))
print(inspect.isclass(MethodLocalDemo))
print(int is type(1))
print(str is type('x'))

try:
    raise _csv.Error('native boom')
except _csv.Error as native_exc:
    print(native_exc.args[0])

from typing_extensions import OrderedDict as TEOrderedDict, Sentinel as TESentinel, TypeVar as TETypeVar
from typing_inspection.introspection import inspect_annotation
import annotated_types

TEBound = TETypeVar('TEBound', covariant=True)
print(TEBound.__name__)
print(type(TESentinel('missing')).__name__)
print(TEOrderedDict.__name__)
print(inspect_annotation(int).type is int)
print(type(annotated_types.Gt(1)).__name__)
