try:
    missing_python_name
except NameError as exc:
    print(type(exc).__name__)
    print(isinstance(exc, Exception))
    print("missing_python_name" in str(exc))
print("quote: \"")
print("back\\slash")
