import asyncio


async def child(value):
    await asyncio.sleep(0)
    return value


async def fail():
    await asyncio.sleep(0)
    raise RuntimeError("boom")


async def main():
    first = asyncio.create_task(child(1))
    second = asyncio.create_task(child(2))
    done, pending = await asyncio.wait([first, second], timeout=0)
    print(len(done), len(pending), first.done(), second.done())
    print(await first, await second)

    bad = asyncio.create_task(fail())
    later = asyncio.create_task(child(3))
    done, pending = await asyncio.wait([bad, later], return_when=asyncio.FIRST_EXCEPTION)
    print(len(done), len(pending), bad.done(), later.done())
    try:
        bad.result()
    except RuntimeError as err:
        print("error", err.args[0])
    if not later.done():
        print(await later)


asyncio.run(main())
