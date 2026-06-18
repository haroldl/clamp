import asyncio


def combine(prefix, value):
    return prefix + str(value)

async def main():
    loop = asyncio.get_running_loop()
    future = loop.run_in_executor(None, combine, "item", 5)
    print(asyncio.isfuture(future), future.done(), future.result())
    print(await loop.run_in_executor(None, combine, "await", 6))

print(asyncio.run(main()))
