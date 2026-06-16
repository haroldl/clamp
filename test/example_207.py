data = {"first": 1, "second": None, "nested": [1], None: "none-key"}

print(data.get("first"), data.get("missing"))
print(data.get("second", 22), data.get("missing", 22))
print(data.get("missing", [3, 4]))
print(data.get(None), data.get("none", None))

nested = data.get("nested")
nested.append(2)
print(data["nested"], nested is data["nested"])

data["first"] = 11
print(data.get("first"))
