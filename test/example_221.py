data = {"first": 1, "second": 2, None: "none", "nested": [3, 4]}
print(data.pop("first"), len(data), data)
print(data.pop("missing", "fallback"), len(data), data)
print(data.pop(None), len(data), data.get(None, "gone"))

nested = data.pop("nested")
nested.append(5)
print(nested, len(data), data)

data["again"] = 9
print(data.pop("again", 0), data.pop("again", 0), len(data), data)
