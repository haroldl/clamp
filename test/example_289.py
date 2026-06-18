import asyncio


async def child():
    await asyncio.sleep(0)
    return 42


async def main():
    loop = asyncio.get_running_loop()
    future = loop.create_future()
    print(future.get_loop() is loop)
    future.set_result("ready")
    print(future.result())

    task = asyncio.create_task(child(), name="alpha")
    print(task.get_loop() is loop)
    print(task.get_name())
    task.set_name("beta")
    print(task.get_name())
    print(await task)

    other_loop = asyncio.new_event_loop()
    other = other_loop.create_task(child(), name="gamma")
    print(other.get_loop() is other_loop, other.get_name())
    print(other_loop.run_until_complete(other))


asyncio.run(main())
