import importlib.util
import sys

class Loader:
    def create_module(self, spec):
        return None

    def exec_module(self, module):
        module.answer = 77

class Finder:
    def __init__(self, entry):
        self.entry = entry

    def find_spec(self, fullname, target=None):
        if fullname == "path_hooked_mod":
            return importlib.util.spec_from_loader(fullname, Loader(), origin=self.entry)
        return None

def hook(entry):
    if entry == "custom-entry":
        return Finder(entry)
    raise ImportError(entry)

sys.path_hooks.insert(0, hook)
sys.path_importer_cache.clear()
sys.path.insert(0, "custom-entry")
spec = importlib.util.find_spec("path_hooked_mod")
print(spec.name)
print(spec.origin)
print("custom-entry" in sys.path_importer_cache)
print(type(sys.path_importer_cache["custom-entry"]).__name__)
import path_hooked_mod
print(path_hooked_mod.answer)
print(path_hooked_mod.__spec__.origin)
