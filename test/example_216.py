data = {"first": 1, "second": [2], "third": 3}
copy = data.copy()
print(copy is data)
print(len(copy), copy)
data["first"] = 10
del data["third"]
copy["added"] = "copy"
copy["second"].append(4)
print(data)
print(copy)

empty = {}
empty_copy = empty.copy()
empty["x"] = 1
print(empty_copy, empty)

import import_value
namespace = import_value.__dict__
namespace_copy = namespace.copy()
print(namespace_copy is namespace)
print("VALUE" in namespace_copy, namespace_copy["VALUE"])
namespace_copy["VALUE"] = "changed"
namespace_copy["EXTRA"] = "extra"
print(import_value.VALUE)
print("EXTRA" in namespace, "EXTRA" in namespace_copy)
