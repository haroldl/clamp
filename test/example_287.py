import asyncio


async def ok(value):
    await asyncio.sleep(0)
    return value


async def fail(label):
    await asyncio.sleep(0)
    raise RuntimeError(label)


async def main():
    results = await asyncio.gather(ok(1), fail("boom"), ok(3), return_exceptions=True)
    print(len(results), results[0], results[2])
    print(isinstance(results[1], RuntimeError), results[1].args[0])

    try:
        await asyncio.gather(ok(4), fail("raise"))
    except RuntimeError as err:
        print("raised", err.args[0])


asyncio.run(main())
