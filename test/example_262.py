import asyncio


seen = []


def record(fut):
    seen.append(fut.result())
    print("callback", fut.result())


def removed(fut):
    print("removed")


async def child():
    await asyncio.sleep(0)
    return 5


async def main():
    loop = asyncio.get_running_loop()
    future = loop.create_future()
    future.add_done_callback(record)
    future.add_done_callback(removed)
    print(future.remove_done_callback(removed))
    future.set_result(3)
    print(future.done(), future.result(), seen)
    future.add_done_callback(record)
    task = asyncio.create_task(child())
    task.add_done_callback(record)
    print(await task)
    print(seen)


asyncio.run(main())
