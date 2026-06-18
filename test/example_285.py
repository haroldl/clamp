import asyncio


async def main():
    barrier = asyncio.Barrier(2)
    print(barrier.parties, barrier.n_waiting, barrier.broken)
    print(await barrier.wait())
    print(barrier.n_waiting)
    print(await barrier.wait())
    print(barrier.n_waiting)

    async with barrier:
        print("inside", barrier.n_waiting)
    print("after", barrier.n_waiting)

    await barrier.abort()
    print(barrier.broken)
    try:
        await barrier.wait()
    except asyncio.BrokenBarrierError as err:
        print("broken", err.args[0])
    await barrier.reset()
    print(barrier.broken, barrier.n_waiting)

    try:
        asyncio.Barrier(0)
    except ValueError as err:
        print("value", err.args[0])


asyncio.run(main())
