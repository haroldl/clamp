total = 0
for value in [1, 2, 3]:
    total += value
print(total)

letters = []
for ch in "ab":
    letters.append(ch)
print(letters)

pairs = []
for item in ("x", "y"):
    pairs.append(item)
print(pairs)

skipped = []
for value in [1, 2, 3, 4]:
    if value == 2:
        continue
    if value == 4:
        break
    skipped.append(value)
else:
    skipped.append(99)
print(skipped)

finished = []
for value in (1, 2):
    finished.append(value)
else:
    finished.append(99)
print(finished)
