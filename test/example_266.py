import asyncio

async def child(value):
    await asyncio.sleep(0)
    return value

async def main():
    loop = asyncio.get_running_loop()
    future = loop.create_future()
    future.set_exception(RuntimeError("boom"))
    err = future.exception()
    print(err.args[0])

    task = asyncio.create_task(child(10))
    print(task.cancel(), task.cancelled(), task.done())
    try:
        await task
    except asyncio.CancelledError:
        print("cancelled")
    print(task.cancelled())

    try:
        await asyncio.wait_for(child(20), timeout=0)
    except asyncio.TimeoutError:
        print("timeout")

    print(await asyncio.wait_for(child(30), timeout=None))

asyncio.run(main())
