import aiohttp
import asyncio

async def main():
    async with aiohttp.ClientSession() as session:
        async with session.get("file:///tmp/clamp-aiohttp-params-missing", params={"q": "hello world", "n": 3}) as resp:
            print(resp.status)
            print(resp.url.endswith("?q=hello+world&n=3"))
    async with aiohttp.patch("file:///tmp/clamp-aiohttp-module-missing") as resp:
        print(resp.method, resp.status)
    async with aiohttp.options("file:///tmp/clamp-aiohttp-module-missing") as resp:
        print(resp.method, resp.status)

asyncio.run(main())
