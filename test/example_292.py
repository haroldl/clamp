import asyncio


async def stream():
    yield "first"
    await asyncio.sleep(0)
    yield "second"


async def main():
    gen = stream()
    print(await gen.asend(None))
    print(await gen.asend(None))
    try:
        await gen.asend("value")
    except TypeError as err:
        print(type(err).__name__, err.args[0])

    gen = stream()
    print(await gen.__anext__())
    print(await gen.aclose())
    print(await anext(gen, "closed"))

    gen = stream()
    try:
        await gen.athrow(ValueError)
    except ValueError as err:
        print(type(err).__name__, err.args)
    print(await anext(gen, "thrown"))


asyncio.run(main())
