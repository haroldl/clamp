import aiohttp
import asyncio


async def show(label, manager):
    async with manager as response:
        print(label, response.method, response.status, await response.text())


async def main():
    async with aiohttp.ClientSession() as session:
        await show("put", session.put("data:text/plain,put"))
        await show("delete", session.delete("data:text/plain,delete"))
        await show("patch", session.patch("data:text/plain,patch"))
        await show("head", session.head("data:text/plain,head"))
        await show("options", session.options("data:text/plain,options"))

    await show("request", aiohttp.request("PATCH", "data:text/plain,top"))
    await show("get", aiohttp.get("data:text/plain,module-get"))
    await show("post", aiohttp.post("data:text/plain,module-post"))


asyncio.run(main())
