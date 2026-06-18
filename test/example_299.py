import asyncio


loop = asyncio.new_event_loop()
seen = []


def first():
    seen.append("first")
    print("first", loop.is_running())
    loop.stop()


def second():
    seen.append("second")
    print("second", loop.is_running())


print(loop.is_running())
loop.call_soon(first)
loop.call_soon(second)
print(loop.run_forever())
print(loop.is_running(), seen)
print(loop.run_forever())
print(loop.is_running(), seen)
loop.call_soon(second)
print(loop.run_forever())
print(seen)
