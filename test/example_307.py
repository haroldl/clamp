import asyncio
from asyncio.exceptions import IncompleteReadError, LimitOverrunError
from asyncio.streams import IncompleteReadError as StreamIncompleteReadError

print(IncompleteReadError is asyncio.IncompleteReadError)
print(StreamIncompleteReadError is asyncio.IncompleteReadError)
print(LimitOverrunError is asyncio.LimitOverrunError)
