import asyncio
from asyncio.runners import Runner

async def child(value):
    await asyncio.sleep(0)
    return value + 1

with asyncio.Runner() as runner:
    loop = runner.get_loop()
    print(loop is runner.get_loop())
    print(runner.run(child(10)))
    print(runner.run(child(20)))

manual = Runner()
print(manual.run(child(30)))
loop = manual.get_loop()
manual.close()
try:
    manual.run(child(40))
except RuntimeError as err:
    print(type(err) is RuntimeError)
print(Runner is asyncio.Runner)
