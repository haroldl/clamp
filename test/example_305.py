import asyncio
import contextvars

user = contextvars.ContextVar("user", default="anon")
request = contextvars.ContextVar("request")

print(user.name, user.get())
try:
    request.get()
except LookupError:
    print("missing")

first = user.set("outer")
print(first.old_value is contextvars.Token.MISSING, user.get())
second = user.set("inner")
print(second.old_value, user.get())
user.reset(second)
print(user.get())
user.reset(first)
print(user.get())

user.set("snapshot")
ctx = contextvars.copy_context()
user.set("current")
print(ctx.get(user), user.get())

def change(value):
    old = user.set(value)
    print("run", user.get())
    user.reset(old)
    return user.get()

print(ctx.run(change, "ctx-value"))
print(ctx.get(user), user.get())
empty = contextvars.Context()
print(empty.get(user, "empty"))

async def main():
    user.set("async")
    await asyncio.sleep(0)
    return user.get()

print(asyncio.run(main()))
