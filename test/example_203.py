data = {"first": 1, "second": 2}
print(len(data), data["first"], data["second"], bool(data))

data["second"] = 22
data["third"] = [3, 4]
print(len(data), data["second"], data["third"][1])
print("first" in data, "missing" in data, "missing" not in data)

duplicate = {"x": 1, "x": 9}
print(len(duplicate), duplicate["x"], duplicate)

empty = {}
print(len(empty), bool(empty), empty)
print(data)
