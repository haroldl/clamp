from . import sub
from .sub import name
print(sub.name)
print(name)
def local_relative_import():
    from .sub import name as local_name
    print(local_name)

local_relative_import()
