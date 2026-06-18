import asyncio
import aiohttp
import aiohttp.client_exceptions as exc
from aiohttp.client_exceptions import ContentTypeError, ClientConnectionError, ClientConnectorError, ClientPayloadError, InvalidURL, TooManyRedirects, ServerTimeoutError

async def main():
    print(ContentTypeError is aiohttp.ContentTypeError)
    print(exc.ClientPayloadError is ClientPayloadError, exc.InvalidURL is InvalidURL, exc.TooManyRedirects is TooManyRedirects)
    print(ClientConnectionError.__name__, ClientConnectorError.__name__, ServerTimeoutError.__name__)

    async with aiohttp.get("data:text/plain,not-json") as resp:
        try:
            await resp.json()
        except ContentTypeError as err:
            print(isinstance(err, aiohttp.ClientResponseError), isinstance(err, aiohttp.ClientError))
            print(err.status, err.request_info["method"], err.request_info["url"])
            print(err.message)

    async with aiohttp.get("data:text/plain,%7B%22text%22%3Atrue%7D") as resp:
        print((await resp.json(content_type=None))["text"])

    async with aiohttp.get("data:application/vnd.api+json,%7B%22ok%22%3Atrue%7D") as resp:
        print((await resp.json())["ok"])

asyncio.run(main())
