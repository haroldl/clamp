text = "caf" + chr(233)
euro = chr(8364)
face = chr(128512)
line = "plain" + chr(10) + "quote"

print(ascii(text))
print(ascii([text, euro, face]))
print(ascii({"name": text, "symbol": euro}))
print(ascii(line))
print(ascii(None), ascii(True), ascii(12))
