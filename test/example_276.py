import asyncio


async def main():
    queue = asyncio.Queue(maxsize=1)
    queue.put_nowait("one")
    try:
        queue.put_nowait("two")
    except asyncio.QueueFull as err:
        print("full", err.args[0])

    print(queue.get_nowait())
    try:
        queue.get_nowait()
    except asyncio.QueueEmpty as err:
        print("empty", err.args[0])


asyncio.run(main())
