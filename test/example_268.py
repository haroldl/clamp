import asyncio
import aiohttp

async def main():
    async with aiohttp.ClientSession() as session:
        async with session.get("file:///tmp/clamp-aiohttp-missing-file-for-raise-status") as resp:
            print(resp.status, resp.ok)
            try:
                resp.raise_for_status()
            except aiohttp.ClientResponseError as err:
                print(isinstance(err, aiohttp.ClientError))
                print(err.status, err.message)
                print(err.request_info["method"], err.request_info["url"])
        async with session.get("data:,ok") as resp:
            print(resp.raise_for_status())

asyncio.run(main())
