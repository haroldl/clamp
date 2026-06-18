import asyncio

loop = asyncio.new_event_loop()
seen = []

def add(value):
    seen.append(value)

print(loop.get_debug())
print(loop.set_debug(True))
print(loop.get_debug())
loop.set_debug(False)
print(loop.get_debug())

handle = loop.call_soon_threadsafe(add, "soon")
print(handle.cancelled())
print(loop.run_forever())
print(seen)

print(loop.run_until_complete(loop.shutdown_asyncgens()))
print(loop.run_until_complete(loop.shutdown_default_executor()))
print(loop.is_closed())
print(loop.close())
print(loop.is_closed())
