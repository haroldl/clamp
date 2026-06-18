import asyncio

class PairStream:
    def __init__(self):
        self.index = 0
        self.values = [("a", 1), ("b", 2)]

    def __aiter__(self):
        return self

    async def __anext__(self):
        if self.index < len(self.values):
            value = self.values[self.index]
            self.index += 1
            return value
        raise StopAsyncIteration()

class PairContext:
    async def __aenter__(self):
        return ("ctx", 3)

    async def __aexit__(self, exc_type, exc, tb):
        return False

async def main():
    async for name, number in PairStream():
        print(name, number)
    async with PairContext() as (label, count):
        print(label, count)

asyncio.run(main())
