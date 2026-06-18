import sys
try:
    import failing_import
except Exception:
    print("failing_import" in sys.modules)
