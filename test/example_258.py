import asyncio
import aiohttp


async def main():
    async with aiohttp.ClientSession() as session:
        async with session.get("data:text/plain,hello%20aiohttp") as resp:
            print(resp.status, resp.reason)
            print(await resp.text())
        async with session.request("GET", "data:,second") as resp:
            print(resp.url)
            print(await resp.text())


asyncio.run(main())
