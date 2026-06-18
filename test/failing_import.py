import sys
print("failing_import" in sys.modules)
raise Exception("boom")
