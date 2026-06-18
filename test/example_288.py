import asyncio


async def child(value):
    await asyncio.sleep(0)
    return value


async def main():
    loop = asyncio.get_running_loop()

    done = loop.create_future()
    done.set_result("ready")
    print(await asyncio.wait_for(done, timeout=0), done.cancelled())

    failed = loop.create_future()
    failed.set_exception(RuntimeError("stored"))
    try:
        await asyncio.wait_for(failed, timeout=0)
    except RuntimeError as err:
        print("stored", err.args[0])

    pending = asyncio.create_task(child(5))
    try:
        await asyncio.wait_for(pending, timeout=0)
    except asyncio.TimeoutError:
        print("timeout", pending.cancelled())


asyncio.run(main())
