import asyncio


class Box:
    kind = "box"

    def __init__(self, value=0):
        self.value = value

    def add(self, amount=1):
        self.value = self.value + amount
        return self.value

    async def __aenter__(self):
        await asyncio.sleep(0)
        self.value = self.value + 10
        return self

    async def __aexit__(self, exc_type, exc, tb):
        await asyncio.sleep(0)
        self.value = self.value + 100
        return False


async def main():
    box = Box(value=5)
    print(box.kind, box.add(amount=2))
    async with box as entered:
        print(entered is box, box.value)
    print(box.value)


asyncio.run(main())
