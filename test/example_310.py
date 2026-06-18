import asyncio
import asyncio.subprocess
from asyncio.subprocess import PIPE, Process, create_subprocess_shell

async def main():
    proc = await asyncio.create_subprocess_exec(
        "/bin/sh",
        "-c",
        "printf out; printf err >&2",
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
    )
    stdout, stderr = await proc.communicate()
    print(stdout, stderr, proc.returncode)
    print(await proc.wait())

    shell = await create_subprocess_shell("printf shell", stdout=PIPE)
    output, err = await shell.communicate()
    print(output, err, shell.returncode)
    print(Process is asyncio.Process)

asyncio.run(main())
