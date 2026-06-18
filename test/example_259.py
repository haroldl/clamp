import asyncio
import aiohttp


async def main():
    async with aiohttp.ClientSession() as session:
        async with session.get("data:application/json,%7B%22ok%22%3Atrue%2C%22items%22%3A%5B1%2C2%2Cnull%5D%2C%22name%22%3A%22clamp%22%7D") as resp:
            print(resp.status, resp.ok, resp.method, resp.content_type)
            data = await resp.json()
            print(data["ok"], data["items"][2], data["name"])
            print(await resp.read())


asyncio.run(main())
