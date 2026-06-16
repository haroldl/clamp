data = {"first": 1, "second": [2], None: "none"}
alias = data
result = data.clear()
print(result is None, len(data), data, alias is data, alias)

data["after"] = 3
print(len(data), data)

empty = {}
print(empty.clear() is None, len(empty), empty)

import import_value
namespace = import_value.__dict__
print("VALUE" in namespace, import_value.VALUE)
print(namespace.clear() is None, len(namespace), namespace)
print("VALUE" in namespace)
namespace["VALUE"] = "restored"
print(import_value.VALUE, namespace)
