import asyncio


async def main():
    priority = asyncio.PriorityQueue()
    await priority.put((3, "low"))
    priority.put_nowait((1, "high"))
    priority.put_nowait((2, "middle"))
    print(await priority.get())
    print(priority.get_nowait())
    print(priority.get_nowait())

    lifo = asyncio.LifoQueue(maxsize=3)
    await lifo.put("first")
    lifo.put_nowait("second")
    lifo.put_nowait("third")
    print(await lifo.get())
    print(lifo.get_nowait())
    print(lifo.get_nowait())


asyncio.run(main())
