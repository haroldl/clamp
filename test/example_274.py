import asyncio

async def main():
    sem = asyncio.Semaphore(value=2)
    print(sem.locked())
    print(await sem.acquire(), sem.locked())
    print(await sem.acquire(), sem.locked())
    print(await sem.acquire())
    sem.release()
    print(sem.locked())
    async with sem:
        print("inside", sem.locked())
    print("outside", sem.locked())

asyncio.run(main())
