try:
    import definitely_missing_module
except ModuleNotFoundError as exc:
    print(type(exc).__name__)
    print(isinstance(exc, ImportError))
    print(exc.name)
    print(exc.path)
    print(str(exc))
