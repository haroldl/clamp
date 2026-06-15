words = ["ab", "pq", "rs"]
print(".".join(words))
print("".join(words))
print("-".join(("left", "right")))
print("|".join([]))
print("|".join(["solo"]))
print(",".join("abc"))
parts = ["a"]
parts.append("b")
print(" ".join(parts))
it = iter(["x", "y"])
print("/".join(it))
print(list(it))
