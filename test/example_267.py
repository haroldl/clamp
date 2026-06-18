import asyncio

async def main():
    event = asyncio.Event()
    print(event.is_set())
    print(await event.wait())
    event.set()
    print(event.is_set(), await event.wait())
    event.clear()
    print(event.is_set())

    queue = asyncio.Queue(maxsize=2)
    print(queue.empty(), queue.full(), queue.qsize())
    await queue.put("alpha")
    queue.put_nowait("beta")
    print(queue.empty(), queue.full(), queue.qsize())
    print(await queue.get())
    queue.task_done()
    print(queue.get_nowait())
    queue.task_done()
    print(queue.empty(), queue.qsize())
    await queue.join()
    print("joined")

asyncio.run(main())
