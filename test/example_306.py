import asyncio
from asyncio.streams import open_connection, StreamReader, StreamWriter

print(callable(open_connection))
print(StreamReader is asyncio.StreamReader)
print(StreamWriter is asyncio.StreamWriter)
