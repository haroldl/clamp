import asyncio
import aiohttp

async def main():
    try:
        async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=0)) as session:
            async with session.get("data:text/plain,expired") as resp:
                print("not reached", resp.status)
    except aiohttp.ServerTimeoutError as err:
        print("session", isinstance(err, aiohttp.ClientConnectionError), err.args[0])

    async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=0)) as session:
        async with session.get("data:text/plain,override", timeout=None) as resp:
            print("override", resp.status, await resp.text())

    async with aiohttp.ClientSession() as session:
        try:
            async with session.get("data:text/plain,request", timeout=0) as resp:
                print("not reached", resp.status)
        except aiohttp.ServerTimeoutError as err:
            print("request", isinstance(err, TimeoutError), err.args[0])

asyncio.run(main())
