import asyncio
import aiohttp
import aiohttp.client
import aiohttp.client_exceptions
from aiohttp.client import ClientSession, request
from aiohttp.client_exceptions import ClientError, ClientResponseError
from aiohttp.connector import TCPConnector
from aiohttp.client_reqrep import ClientResponse

async def main():
    print(aiohttp.client.ClientSession is aiohttp.ClientSession)
    print(ClientSession is aiohttp.ClientSession)
    print(ClientError is aiohttp.ClientError)
    print(ClientResponseError is aiohttp.ClientResponseError)
    print(aiohttp.client_exceptions.ClientResponseError is ClientResponseError)
    print(TCPConnector is aiohttp.TCPConnector)
    print(ClientResponse is aiohttp.ClientResponse)
    async with request("GET", "data:text/plain,submodule") as resp:
        print(resp.status, await resp.text())
    try:
        async with ClientSession() as session:
            async with session.get("file:///tmp/clamp-aiohttp-submodule-missing") as resp:
                resp.raise_for_status()
    except ClientResponseError as err:
        print(isinstance(err, ClientError), err.status, err.message)

asyncio.run(main())
