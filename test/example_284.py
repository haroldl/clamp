import asyncio


policy = asyncio.get_event_loop_policy()
print(policy is asyncio.get_event_loop_policy())

default_loop = asyncio.get_event_loop()
print(default_loop is policy.get_event_loop())

replacement = policy.new_event_loop()
asyncio.set_event_loop(replacement)
print(asyncio.get_event_loop() is replacement)

other_policy = asyncio.get_event_loop_policy()
other_loop = other_policy.new_event_loop()
other_policy.set_event_loop(other_loop)
print(asyncio.get_event_loop() is other_loop)


async def main():
    running = asyncio.get_running_loop()
    print(asyncio.get_event_loop() is other_loop)
    print(policy.get_event_loop() is running)


asyncio.run(main())
asyncio.set_event_loop_policy(None)
print(asyncio.get_event_loop_policy() is policy)
