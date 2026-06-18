import asyncio


async def values():
    for item in [1, 2, 3]:
        await asyncio.sleep(0)
        yield item


async def pairs():
    yield ("a", 10)
    yield ("b", 20)


async def main():
    async for item in values():
        seen = item
    print(item, seen)

    async for name, count in pairs():
        label = name
    print(name, count, label)

    for regular in [4, 5]:
        total = regular
    print(regular, total)


asyncio.run(main())
