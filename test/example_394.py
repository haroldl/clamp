def f(config):
    if config.get("skip"):
        pass
    if (populate_by_name := config.get("populate_by_name")) is not None:
        if config.get("validate_by_name") is None:
            config["validate_by_name"] = populate_by_name
    return config.get("validate_by_name")

print(f({"populate_by_name": True}))
print(f({"populate_by_name": False}))
print(f({"skip": True, "populate_by_name": True}))
print(f({}))
