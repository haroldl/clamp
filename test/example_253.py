import asyncio

async def answer():
    await asyncio.sleep(0)
    return 42

print(asyncio.run(answer()))
