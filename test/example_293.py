import asyncio


async def child(value):
    await asyncio.sleep(0)
    return value


async def main():
    first = asyncio.create_task(child(1))
    second = asyncio.create_task(child(2))
    total = 0
    for completed in asyncio.as_completed([first, second]):
        print(type(completed).__name__, await completed)
        total += 1
    print("count", total)

    iterator = asyncio.as_completed([child(3)])
    task = next(iterator)
    print(await task)
    try:
        next(iterator)
    except StopIteration:
        print("stopped")


asyncio.run(main())
