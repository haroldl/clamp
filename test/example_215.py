data = {"first": 1, "second": 2, "third": 3}
alias = data
del data["second"]
print(len(data), data, alias is data)
print("second" in data, data.get("second", "missing"))

print(data.__delitem__("first"), data)
data["first"] = 11
print(data)

nested = {"items": [1, 2], None: "none-key"}
removed = nested["items"]
del nested["items"]
removed.append(3)
print(nested, removed)
del nested[None]
print(len(nested), bool(nested), nested)

import import_value
namespace = import_value.__dict__
print("VALUE" in namespace, import_value.VALUE)
del namespace["VALUE"]
print("VALUE" in namespace, namespace.get("VALUE", "missing"))
