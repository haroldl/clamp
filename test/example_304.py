import asyncio
from asyncio.locks import Lock, Event, Semaphore
from asyncio.queues import Queue, QueueEmpty
from asyncio.runners import run
from asyncio.timeouts import timeout
from asyncio.taskgroups import TaskGroup
from asyncio.events import new_event_loop, set_event_loop, get_event_loop
from asyncio.coroutines import iscoroutine, iscoroutinefunction

async def child(value):
    await asyncio.sleep(0)
    return value

async def main():
    lock = Lock()
    async with lock:
        print(lock.locked())
    event = Event()
    print(event.is_set())
    event.set()
    print(await event.wait())
    sem = Semaphore(1)
    print(await sem.acquire())
    queue = Queue()
    queue.put_nowait("item")
    print(await queue.get())
    try:
        queue.get_nowait()
    except QueueEmpty:
        print("empty")
    async with timeout(None):
        print(await child(5))
    async with TaskGroup() as group:
        task = group.create_task(child(7))
    print(task.result())
    print(iscoroutine(child(9)), iscoroutinefunction(child))

loop = new_event_loop()
set_event_loop(loop)
print(get_event_loop() is loop)
print(run(main()))
