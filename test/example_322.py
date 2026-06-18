import aiohttp
from aiohttp.client import FormData

form = FormData()
print(form.add_fields(("first", "one"), {"second": "two"}, [("third", "three")]))
form.add_field("fourth", "four")
print(form.is_multipart)

# Exercise the body path through a file URL request context so headers are built.
import asyncio

async def main():
    async with aiohttp.ClientSession() as session:
        async with session.post("file:///tmp/clamp-form-add-fields-missing", data=form) as resp:
            print(resp.request_info["headers"]["Content-Type"])
            print(resp.request_info["headers"]["Content-Length"])

asyncio.run(main())
