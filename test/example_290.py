import aiohttp
import asyncio


async def main():
    timeout = aiohttp.ClientTimeout(total=5, connect=1, sock_read=2, sock_connect=3, ceil_threshold=7)
    print(timeout.total, timeout.connect, timeout.sock_read, timeout.sock_connect, timeout.ceil_threshold)

    positional_timeout = aiohttp.ClientTimeout(9)
    print(positional_timeout.total, positional_timeout.connect)

    connector = aiohttp.TCPConnector(ssl=False, limit=12, limit_per_host=4, force_close=True)
    print(connector.ssl, connector.limit, connector.limit_per_host, connector.force_close, connector.closed)
    print(connector.close())
    print(connector.closed)

    async with aiohttp.ClientSession(timeout=timeout, connector=connector) as session:
        async with session.get("data:text/plain,ok") as response:
            print(response.status, await response.text())


asyncio.run(main())
