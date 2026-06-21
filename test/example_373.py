import sys

def collect(**kwargs):
    print(sorted(kwargs.items()))

collect(SKU="A1", normal_name=2)

module = sys.modules[__name__]

def __getattr__(name):
    if name == "dynamic":
        return "value"
    raise AttributeError(name)

print(getattr(module, "dynamic"))
print(getattr(module, "missing", "fallback"))
