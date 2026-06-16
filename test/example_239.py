data = {"first": 1, "second": 2}
alias = data
other = {"second": "two", "third": 3}
result = data.update(other)
print(result is None)
print(data)
print(alias is data, alias)

other["third"] = "changed"
other["fourth"] = 4
print(data)

empty = {}
print(empty.update() is None, empty)
print(data.update({}) is None, data)
print(data.update(data) is None, data)

nested = ["value"]
data.update({"nested": nested})
print(data)
nested.append("mutated")
print(data)

import import_value
namespace = import_value.__dict__
print(import_value.VALUE)
print(namespace.update({"VALUE": "updated", "ADDED_BY_UPDATE": "added"}) is None)
print(import_value.VALUE, import_value.ADDED_BY_UPDATE, namespace["ADDED_BY_UPDATE"])
