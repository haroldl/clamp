import asyncio

async def child(value):
    await asyncio.sleep(0)
    return value + 1

async def main():
    first = await child(1)
    second = await child(2)
    both = await asyncio.gather(child(10), child(20))
    task = asyncio.create_task(child(30))
    third = await task
    print(first, second, both, third)
    return 99

print(asyncio.run(main()))
