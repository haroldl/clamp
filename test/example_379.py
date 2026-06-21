print(issubclass(RecursionError, RuntimeError))
try:
    raise RecursionError("deep")
except RuntimeError as exc:
    print(type(exc).__name__)
    print(str(exc))
