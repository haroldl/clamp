import asyncio


async def main():
    loop = asyncio.get_running_loop()
    seen = []

    def record(label):
        seen.append(label)
        print("callback", label)

    first = loop.call_soon(record, "first")
    cancelled = loop.call_soon(record, "cancelled")
    later = loop.call_later(5, record, "later")
    print(type(first).__name__, first.cancelled())
    print(cancelled.cancel())
    print(cancelled.cancelled())
    print(type(later).__name__, later.cancelled())

    await asyncio.sleep(0)
    print(seen)

    print(later.cancel())
    print(later.cancelled())
    await asyncio.sleep(0)
    print(seen)


asyncio.run(main())
