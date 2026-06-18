import asyncio
import aiohttp

async def main():
    async with aiohttp.get("data:text/plain,abcdef") as resp:
        print(resp.content.read_nowait(2))
        chunk, end = await resp.content.readchunk()
        print(chunk, end, resp.content.is_eof(), resp.content.exception())

    async with aiohttp.get("data:text/plain,xyz") as resp:
        items = []
        async for chunk in resp.content.iter_any():
            items.append(chunk)
        print(items, resp.content.at_eof())

    async with aiohttp.get("data:text/plain,12345") as resp:
        pairs = []
        async for chunk, end in resp.content.iter_chunks():
            pairs.append((chunk, end))
        print(pairs)

asyncio.run(main())
