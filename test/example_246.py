left = {"first": 1, "second": 2}
same = {"second": 2, "first": 1}
print(left == same, left != same)

print(left == {"first": 1})
print(left == {"first": 1, "second": 3})
print(left != {"first": 1, "second": 3})

nested = {"items": [1, 2], "meta": {"ok": True}}
other = {"meta": {"ok": 1}, "items": [1, 2]}
print(nested == other)

alias = nested
print(alias == nested, alias is nested)
alias["items"].append(3)
print(nested == other, nested != other)
other["items"].append(3)
print(nested == other)

print({} == {}, {} != {})
print({"none": None} == {"none": None})
