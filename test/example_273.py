import asyncio

async def child(value):
    await asyncio.sleep(0)
    return value

async def main():
    first = asyncio.create_task(child(1))
    second = asyncio.create_task(child(2))
    done, pending = await asyncio.wait([first, second])
    print(len(done), len(pending), first.done(), second.done())
    print(first.result(), second.result())
    third = asyncio.create_task(child(3))
    done, pending = await asyncio.wait([third], return_when=asyncio.FIRST_COMPLETED)
    print(len(done), len(pending), third.result())

asyncio.run(main())
