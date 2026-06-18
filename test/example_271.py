import asyncio

async def pairs():
    yield ("a", 1)
    await asyncio.sleep(0)
    yield ("b", 2)
    yield ("c", 3)

async def scale(value):
    await asyncio.sleep(0)
    return value * 10

async def main():
    sync_map = {key: value + 1 for key, value in [("x", 1), ("y", 2)] if value > 1}
    async_map = {key: await scale(value) async for key, value in pairs() if value > 1}
    print(sync_map["y"], len(sync_map))
    print(async_map["b"], async_map["c"], len(async_map))

asyncio.run(main())
