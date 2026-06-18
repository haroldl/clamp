import asyncio


async def child(value):
    await asyncio.sleep(0)
    return value + 5


async def main():
    loop = asyncio.get_running_loop()
    future = asyncio.run_coroutine_threadsafe(child(10), loop)
    print(future.done(), future.result())

    other_loop = asyncio.new_event_loop()
    other = asyncio.run_coroutine_threadsafe(child(20), other_loop)
    print(other.done(), other.result())

    try:
        asyncio.run_coroutine_threadsafe(123, loop)
    except TypeError as err:
        print("type", err.args[0])


asyncio.run(main())
