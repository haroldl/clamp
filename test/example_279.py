import asyncio


async def child(value):
    await asyncio.sleep(0)
    return value * 2


async def main():
    tasks = []
    async with asyncio.TaskGroup() as group:
        tasks.append(group.create_task(child(10), name="first"))
        tasks.append(group.create_task(child(20)))
        print(tasks[0].done(), tasks[1].done())
    print(tasks[0].done(), tasks[1].done())
    print(tasks[0].result(), tasks[1].result())

    group = asyncio.TaskGroup()
    try:
        group.create_task(child(1))
    except RuntimeError as err:
        print("closed", err.args[0])


asyncio.run(main())
