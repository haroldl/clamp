import asyncio

async def child(value):
    await asyncio.sleep(0)
    return value

async def main():
    loop = asyncio.get_event_loop()
    print(loop is asyncio.get_running_loop())
    print(asyncio.current_task() is None)
    print(len(asyncio.all_tasks()))
    task = asyncio.ensure_future(child(7))
    print(task.done())
    print(len(asyncio.all_tasks()))
    same = asyncio.ensure_future(task)
    print(same is task)
    print(await asyncio.shield(task))
    print(task.done(), task.result())
    print(await asyncio.wait_for(child(9), timeout=1))

asyncio.run(main())
