import asyncio


async def main():
    loop = asyncio.get_running_loop()
    seen = []

    def record(label):
        seen.append(label)
        print("timer", label)

    start = loop.time()
    later = loop.call_later(5, record, "later")
    at = loop.call_at(start + 10, record, "at")
    cancelled = loop.call_at(start + 20, record, "cancelled")
    print(type(later).__name__, later.when() >= start + 5)
    print(type(at).__name__, at.when() == start + 10)
    print(cancelled.cancel())
    print(cancelled.cancelled())

    await asyncio.sleep(0)
    print(seen)


asyncio.run(main())
