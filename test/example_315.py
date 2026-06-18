import asyncio
import aiohttp
from aiohttp.client_exceptions import ClientResponseError

async def main():
    try:
        async with aiohttp.ClientSession(raise_for_status=True) as session:
            async with session.get("file:///tmp/clamp-aiohttp-raise-default-missing") as resp:
                print("not reached", resp.status)
    except ClientResponseError as err:
        print("default", err.status, err.message)

    async with aiohttp.ClientSession(raise_for_status=True) as session:
        async with session.get("file:///tmp/clamp-aiohttp-raise-override-missing", raise_for_status=False) as resp:
            print("override", resp.status)

    async with aiohttp.ClientSession(raise_for_status=False) as session:
        try:
            async with session.get("file:///tmp/clamp-aiohttp-raise-request-missing", raise_for_status=True) as resp:
                print("not reached", resp.status)
        except aiohttp.ClientResponseError as err:
            print("request", err.status)

asyncio.run(main())
