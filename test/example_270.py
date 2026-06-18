import asyncio

async def nums():
    yield 1
    await asyncio.sleep(0)
    yield 2
    yield 3

async def scale(value):
    await asyncio.sleep(0)
    return value * 10

async def main():
    sync_values = [value + 1 for value in [1, 2, 3] if value > 1]
    async_values = [await scale(value) async for value in nums() if value > 1]
    print(sync_values[0], sync_values[1], len(sync_values))
    print(async_values[0], async_values[1], len(async_values))

asyncio.run(main())
