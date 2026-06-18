import aiohttp
from aiohttp.client import BasicAuth, FormData

first = aiohttp.BasicAuth("user", "pass")
second = BasicAuth(login="name", password="secret", encoding="utf-8")
print(first.login, first.password, first.encoding)
print(first.encode())
print(second.encode())
print(BasicAuth is aiohttp.BasicAuth)
print(FormData is aiohttp.FormData)

form = FormData({"name": "clamp"})
print(form.is_multipart)
print(form.add_field("space", "hello world"))
