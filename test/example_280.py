import asyncio


async def main():
    scopes = []
    async with asyncio.timeout(None) as scope:
        scopes.append(scope)
        print(scope.when(), scope.expired())
        scope.reschedule(asyncio.get_running_loop().time() + 10)
        print(scope.expired())
    print(scopes[0].expired())

    try:
        async with asyncio.timeout(0):
            print("unreachable")
    except asyncio.TimeoutError:
        print("timeout")

    loop = asyncio.get_running_loop()
    try:
        async with asyncio.timeout_at(loop.time() - 1):
            print("past")
    except TimeoutError:
        print("timeout_at")


asyncio.run(main())
