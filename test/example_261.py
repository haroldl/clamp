import asyncio


async def fail(kind):
    await asyncio.sleep(0)
    if kind:
        raise RuntimeError("boom")
    return 7


async def main():
    try:
        print(await fail(True))
    except RuntimeError as err:
        print(type(err).__name__, err.args[0])
    else:
        print("not reached")
    finally:
        print("cleanup")

    try:
        value = await fail(False)
    except TypeError:
        print("wrong")
    else:
        print("else", value)


asyncio.run(main())
