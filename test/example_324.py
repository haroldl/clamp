import asyncio
import aiohttp
from aiohttp.client_reqrep import ClientWebSocketResponse, WSMessage


async def main():
    print(ClientWebSocketResponse is aiohttp.ClientWebSocketResponse)
    print(WSMessage is aiohttp.WSMessage)
    print(aiohttp.WSMsgType.TEXT, aiohttp.WSMsgType.CLOSED)

    async with aiohttp.ClientSession() as session:
        async with session.ws_connect("data:text/plain,first") as ws:
            print(ws.closed)
            print(await ws.receive_str())
            print(await ws.send_str("second"))
            message = await ws.receive()
            print(message.type == aiohttp.WSMsgType.TEXT, message.data)
            print(await ws.close(), ws.closed)

        other = await session.ws_connect("data:text/plain,awaited")
        print(await other.receive_str())

    async with aiohttp.ws_connect("data:text/plain,module") as ws:
        print(await ws.receive_str())


asyncio.run(main())
