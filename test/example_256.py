import asyncio

def add(a, b=10):
    return a + b

async def child(value, amount=1):
    await asyncio.sleep(0, result=None)
    return value + amount

async def main():
    print(add(1), add(a=2, b=3), add(4, b=5))
    print(await child(value=10, amount=2))
    task = asyncio.create_task(child(20, amount=3), name="worker")
    print(await task)
    print(await asyncio.gather(child(1, amount=4), return_exceptions=False))
    return await asyncio.sleep(0, result=99)

print(asyncio.run(main(), debug=False))
