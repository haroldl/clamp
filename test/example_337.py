import import_value

try:
    import_value.__loader__.get_data("/tmp/clamp_missing_file_for_exception_test")
except FileNotFoundError as exc:
    print(type(exc).__name__)
    print(isinstance(exc, OSError))
    print(str(exc))
