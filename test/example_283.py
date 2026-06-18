import asyncio


async def child():
    return 10


async def stream():
    yield 1


def regular():
    return 20


async def main():
    coro = child()
    print(asyncio.iscoroutine(coro), asyncio.isfuture(coro))
    task = asyncio.create_task(coro)
    print(asyncio.isfuture(task), asyncio.iscoroutine(task))
    print(await task)

    loop = asyncio.get_running_loop()
    future = loop.create_future()
    future.set_result("ready")
    print(asyncio.isfuture(future), future.result())

    print(asyncio.iscoroutinefunction(child), asyncio.iscoroutinefunction(regular))
    agen = stream()
    print(asyncio.iscoroutine(agen))


asyncio.run(main())
