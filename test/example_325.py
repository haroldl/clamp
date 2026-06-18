import asyncio
import contextlib
from contextlib import asynccontextmanager
import inspect


def identity(fn):
    print("decorating function")
    return fn


@identity
def plain(value):
    return value + 1


@asynccontextmanager
async def resource(name):
    print("enter", name)
    yield name + "-value"


@contextlib.asynccontextmanager
async def second():
    yield "second"


async def main():
    print(plain(4))
    print(callable(resource), inspect.isasyncgenfunction(resource))
    async with resource("db") as value:
        print(value)
    async with second() as value:
        print(value)


asyncio.run(main())
