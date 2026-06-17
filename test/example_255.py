import asyncio

async def child(value):
    await asyncio.sleep(0)
    return value

async def main():
    lock = asyncio.Lock()
    async with lock:
        print(lock.locked())
    print(lock.locked())
    first = asyncio.create_task(child(1))
    second = asyncio.create_task(child(2))
    async for task in asyncio.as_completed([first, second]):
        print(type(task).__name__, await task)

asyncio.run(main())
