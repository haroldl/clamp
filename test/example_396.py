from types import FunctionType, MethodType


def f(value):
    return value


print(isinstance(f, FunctionType), isinstance(f, MethodType))
