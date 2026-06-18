import asyncio
import aiohttp

async def main():
    async with aiohttp.get("data:text/plain,abcdef") as resp:
        print(await resp.content.read(2))
        print(await resp.content.readany())
        print(resp.content.at_eof())

    async with aiohttp.get("data:text/plain,line1%0Aline2") as resp:
        print(await resp.content.readline())
        print(await resp.content.readexactly(5))
        print(resp.content.at_eof())

    async with aiohttp.get("data:text/plain,abcdefg") as resp:
        chunks = []
        async for chunk in resp.content.iter_chunked(3):
            chunks.append(chunk)
        print(chunks)
        print(resp.content.at_eof())

asyncio.run(main())
