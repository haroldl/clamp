import asyncio


ready = True


def predicate():
    return ready


async def main():
    cond = asyncio.Condition()
    print(cond.locked())
    await cond.acquire()
    print(cond.locked())
    print(await cond.wait())
    print(cond.locked())
    cond.notify()
    cond.notify_all()
    cond.release()
    print(cond.locked())

    async with cond:
        print("with", cond.locked(), await cond.wait_for(predicate))

    try:
        cond.notify()
    except RuntimeError as err:
        print("notify", err.args[0])


asyncio.run(main())
