def plain():
    return None


def configured(left, right=3, *, mode="x", flag):
    return left, right, mode, flag


plain.answer = 42
namespace = plain.__dict__
print(namespace is plain.__dict__)
print(namespace["answer"])
namespace["extra"] = "value"
print(plain.extra)
plain.later = "setattr"
print(namespace["later"])
print(configured.__defaults__)
print(configured.__kwdefaults__["mode"], "flag" in configured.__kwdefaults__)
