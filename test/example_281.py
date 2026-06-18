import asyncio


def combine(prefix, value, suffix="!"):
    return prefix + str(value) + suffix


async def main():
    print(await asyncio.to_thread(combine, "item", 7))
    print(await asyncio.to_thread(combine, "item", 8, suffix="?"))
    task = asyncio.create_task(asyncio.to_thread(combine, "task", 3, suffix="."))
    print(await task)


asyncio.run(main())
