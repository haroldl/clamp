def greet(name, punctuation="!"):
    print("hello " + name + punctuation)

greet("clamp")
greet("python", "?")

start = 10

def add(value, amount=start + 1):
    return value + amount

start = 99
print(add(1), add(1, 2))

def collect(value, bucket=[]):
    bucket.append(value)
    return bucket

print(collect(1))
print(collect(2))
print(collect(3, []))
print(collect(4))
