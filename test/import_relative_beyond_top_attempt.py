try:
    import relative_pkg.beyond
except ImportError as exc:
    print(type(exc).__name__)
    print(str(exc))
