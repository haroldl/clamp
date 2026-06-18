import asyncio


async def main():
    sem = asyncio.BoundedSemaphore(value=1)
    print(await sem.acquire(), sem.locked())
    sem.release()
    print(sem.locked())
    try:
        sem.release()
    except ValueError as err:
        print("over", err.args[0])

    sem2 = asyncio.BoundedSemaphore(value=1)
    async with sem2:
        print("inside", sem2.locked())
    print("done", sem2.locked())


asyncio.run(main())
