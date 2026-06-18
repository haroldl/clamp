import asyncio
import aiohttp

async def main():
    async with aiohttp.ClientSession() as session:
        resp = await session.get("data:text/plain,awaited")
        print(resp.status, await resp.text())
        print(resp.closed)
        print(resp.release(), resp.closed)

    resp = await aiohttp.post("data:text/plain,module-await")
    print(resp.method, resp.status, await resp.text())

    try:
        await aiohttp.get("file:///tmp/clamp-aiohttp-await-missing", raise_for_status=True)
    except aiohttp.ClientResponseError as err:
        print("raised", err.status, err.message)

asyncio.run(main())
