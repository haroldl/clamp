import asyncio
import inspect
import aiohttp


async def child():
    return 5


async def stream():
    yield 1


def regular():
    return 0


class Box:
    async def method(self):
        return 8

    async def events(self):
        yield 9


print(inspect.iscoroutinefunction(child), inspect.isasyncgenfunction(child))
print(inspect.iscoroutinefunction(stream), inspect.isasyncgenfunction(stream))
print(inspect.iscoroutinefunction(regular), inspect.isasyncgenfunction(regular))

coro = child()
agen = stream()
print(inspect.iscoroutine(coro), inspect.isawaitable(coro))
print(inspect.isasyncgen(agen), inspect.isawaitable(agen))

box = Box()
print(inspect.iscoroutinefunction(box.method), inspect.isasyncgenfunction(box.events))


async def main():
    async with aiohttp.ClientSession() as session:
        request = session.get("data:text/plain,ok")
        print(inspect.isawaitable(request))
        response = await request
        print(await response.text())
    print(await box.method())


asyncio.run(main())
