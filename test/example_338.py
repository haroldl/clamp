import import_value

try:
    import_value.missing_attr
except AttributeError as exc:
    print(type(exc).__name__)
    print(isinstance(exc, Exception))
    print("missing_attr" in str(exc))
