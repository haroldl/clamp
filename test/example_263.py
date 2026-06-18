import asyncio

class Manager:
    def __init__(self, name):
        self.name = name

    async def __aenter__(self):
        print("enter", self.name)
        return self.name

    async def __aexit__(self, exc_type, exc, tb):
        print("exit", self.name, exc_type is None)
        return False

async def main():
    async with Manager("outer") as outer, Manager("inner") as inner:
        print("body", outer, inner)

asyncio.run(main())
