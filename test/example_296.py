import asyncio


async def child(value):
    await asyncio.sleep(0)
    return value


async def main():
    running = asyncio.get_running_loop()
    future = asyncio.Future(loop=running)
    print(asyncio.isfuture(future), future.get_loop() is running)
    future.set_result("done")
    print(await future)

    task = asyncio.Task(child(10), loop=running, name="ctor", context=None)
    print(task.get_loop() is running, task.get_name())
    print(await task)

    other_loop = asyncio.new_event_loop()
    other_future = asyncio.Future(other_loop)
    print(other_future.get_loop() is other_loop)
    other_future.set_result("other")
    print(other_loop.run_until_complete(other_future))

    other_task = asyncio.Task(child(20), other_loop, name="positional")
    print(other_task.get_loop() is other_loop, other_task.get_name())
    print(other_loop.run_until_complete(other_task))


asyncio.run(main())
