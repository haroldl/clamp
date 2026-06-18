import asyncio
from asyncio.streams import start_server, Server

print(callable(asyncio.start_server))
print(start_server is asyncio.start_server)
print(Server is asyncio.Server)
