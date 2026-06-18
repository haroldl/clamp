import asyncio
import aiohttp


async def main():
    async with aiohttp.ws_connect("data:text/plain,loop") as ws:
        async for msg in ws:
            print(msg.type == aiohttp.WSMsgType.TEXT, msg.data)
        print(ws.closed)

    async with aiohttp.ws_connect("data:text/plain,") as ws:
        print(await ws.send_bytes(b"abc"))
        data = await ws.receive_bytes()
        print(data, len(data))
        await ws.send_str("text")
        try:
            await ws.receive_bytes()
        except TypeError as err:
            print(type(err) is TypeError)


asyncio.run(main())
