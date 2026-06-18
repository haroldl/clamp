import aiohttp
import aiohttp.hdrs as hdrs
from aiohttp.hdrs import METH_GET, METH_POST, CONTENT_TYPE, AUTHORIZATION, SET_COOKIE, LOCATION
from aiohttp.helpers import BasicAuth

print(METH_GET, METH_POST, hdrs.METH_PATCH, hdrs.METH_DELETE)
print(CONTENT_TYPE, AUTHORIZATION, SET_COOKIE, LOCATION)
print(BasicAuth is aiohttp.BasicAuth, BasicAuth("user", "pass").encode())
print("METH_HEAD" in hdrs.__all__, "USER_AGENT" in hdrs.__all__)
