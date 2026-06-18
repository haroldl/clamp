import asyncio


class Suppress:
    async def __aenter__(self):
        print("enter")
        return self

    async def __aexit__(self, exc_type, exc, tb):
        print(exc_type.__name__, exc is None, tb is None)
        return True


async def main():
    async with Suppress():
        raise RuntimeError("boom")
    print("after")


asyncio.run(main())
