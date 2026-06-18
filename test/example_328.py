import asyncio
import aiohttp


async def handle(reader, writer):
    writer.write(b"one\ntwo\n")
    await writer.drain()
    writer.close()
    await writer.wait_closed()


async def main():
    async with aiohttp.get("data:text/plain,a%0Ab%0A") as resp:
        async for line in resp.content:
            print(line)

    server = await asyncio.start_server(handle, "127.0.0.1", 0)
    print(server.sockets[0][0])
    port = server.sockets[0][1]
    reader, writer = await asyncio.open_connection("127.0.0.1", port)
    print(writer.get_extra_info("sockname")[0])
    async for line in reader:
        print(line)
    writer.close()
    await writer.wait_closed()
    server.close()
    await server.wait_closed()


asyncio.run(main())
