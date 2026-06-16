data = {"first": 1, "second": 2}
print(data.setdefault("first", 10))

missing_default = ["new"]
returned = data.setdefault("third", missing_default)
print(returned is missing_default, data)

returned.append("changed")
print(data)

print(data.setdefault(None))
print(data)

print(data.setdefault("nonevalue") is None)
print(data)

import import_value
namespace = import_value.__dict__
print(namespace.setdefault("VALUE", "changed"), import_value.VALUE)
print(namespace.setdefault("ADDED", "setdefault"), import_value.ADDED, namespace["ADDED"])
