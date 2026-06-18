import aiohttp
import asyncio


async def main():
    session = aiohttp.ClientSession()
    print(session.closed)
    async with session.get("data:text/plain,lifecycle") as response:
        print(response.closed, await response.text())
        print(response.release())
        print(response.closed)
    print(session.closed)
    close_result = session.close()
    print(session.closed, type(close_result).__name__)
    print(await close_result)
    print(session.closed)

    async with aiohttp.ClientSession() as managed:
        print("managed", managed.closed)
    print("managed", managed.closed)


asyncio.run(main())
