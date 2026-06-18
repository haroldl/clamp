import asyncio

async def nums():
    yield 1
    await asyncio.sleep(0)
    yield 2

async def main():
    iterator = aiter(nums())
    print(aiter(iterator) is iterator)
    print(await anext(iterator))
    print(await anext(iterator))
    print(await anext(iterator, 99))
    try:
        await anext(iterator)
    except StopAsyncIteration:
        print("stopped")

asyncio.run(main())
