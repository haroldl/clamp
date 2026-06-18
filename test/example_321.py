import asyncio
import aiohttp

async def main():
    resp = await aiohttp.get("data:text/plain,close")
    print(resp.closed)
    print(resp.close(), resp.closed)
    print(await resp.text())

    async with aiohttp.get("data:text/plain,wait") as resp:
        print(resp.closed)
        print(await resp.wait_for_close(), resp.closed)

asyncio.run(main())
