import asyncio
import asyncio.exceptions as exc
from asyncio.futures import Future, isfuture
from asyncio.tasks import create_task, gather, wait, FIRST_COMPLETED

async def child(value):
    await asyncio.sleep(0)
    return value

async def main():
    loop = asyncio.get_running_loop()
    fut = Future(loop=loop)
    fut.set_result("future")
    print(isfuture(fut), fut.result())

    task = create_task(child("task"))
    print(await task)

    first = create_task(child(1))
    second = create_task(child(2))
    done, pending = await wait([first, second], return_when=FIRST_COMPLETED)
    print(len(done), len(pending))
    print(await gather(child(3), child(4)))

    try:
        raise exc.CancelledError("stop")
    except asyncio.CancelledError as err:
        print(type(err) is exc.CancelledError)

asyncio.run(main())
