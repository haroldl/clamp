import asyncio
import aiohttp

async def main():
    owned = aiohttp.TCPConnector()
    session = aiohttp.ClientSession(connector=owned)
    print(session.connector is owned, session.connector_owner, owned.closed)
    print(await session.close())
    print(session.closed, owned.closed)

    shared = aiohttp.TCPConnector()
    session = aiohttp.ClientSession(connector=shared, connector_owner=False)
    print(session.connector is shared, session.connector_owner, shared.closed)
    print(await session.close())
    print(session.closed, shared.closed)
    print(shared.close(), shared.closed)

    detached = aiohttp.TCPConnector()
    session = aiohttp.ClientSession(connector=detached)
    print(session.detach())
    print(session.closed, session.connector is None, detached.closed)

asyncio.run(main())
