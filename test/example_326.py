import asyncio
import aiohttp
from contextlib import AsyncExitStack, aclosing, nullcontext


async def stream():
    yield "item"


async def main():
    async with aclosing(stream()) as gen:
        async for value in gen:
            print(value)

    async with nullcontext("ready") as value:
        print(value)

    stack = AsyncExitStack()
    async with stack:
        session = await stack.enter_async_context(aiohttp.ClientSession())
        response = await stack.enter_async_context(session.get("data:text/plain,stack"))
        print(await response.text())
        print(session.closed, response.closed)
    print(session.closed, response.closed)

    stack = AsyncExitStack()
    session = await stack.enter_async_context(aiohttp.ClientSession())
    await stack.aclose()
    print(session.closed)


asyncio.run(main())
