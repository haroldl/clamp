import import_value

print(import_value.__dict__["VALUE"])
import_value.EXTRA = "attribute value"
print(import_value.__dict__["EXTRA"])
import_value.__dict__["ADDED"] = "dict value"
print(import_value.ADDED)
print(import_value.__dict__ is import_value.__dict__)
print("VALUE" in import_value.__dict__)
print("__dict__" in import_value.__dict__)
