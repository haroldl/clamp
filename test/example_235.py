import import_value

spec = import_value.__spec__
print(spec.has_location)
print(spec._set_fileattr)
spec._set_fileattr = "private-location"
print(spec.has_location)
print(spec._set_fileattr)
print(bool(spec.has_location))
spec.has_location = ""
print(spec.has_location)
print(spec._set_fileattr)
spec.has_location = "nonempty"
print(spec.has_location)
print(spec._set_fileattr)
