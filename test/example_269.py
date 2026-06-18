import asyncio

async def pairs():
    yield ("a", 1)
    await asyncio.sleep(0)
    yield ("b", 2)

class Source:
    async def values(self):
        yield 3
        await asyncio.sleep(0)
        yield 4

async def main():
    async for name, value in pairs():
        print(name, value)
    total = 0
    async for value in Source().values():
        total += value
    print(total)

asyncio.run(main())
