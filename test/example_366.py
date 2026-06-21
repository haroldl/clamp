import importlib.util
import sys

class Loader:
    def create_module(self, spec):
        return None

    def exec_module(self, module):
        module.answer = 42
        module.loaded_by = type(self).__name__

class Finder:
    def find_spec(self, fullname, path=None, target=None):
        if fullname == "hooked_mod":
            return importlib.util.spec_from_loader(fullname, Loader(), origin="hooked")
        return None

sys.meta_path.insert(0, Finder())
spec = importlib.util.find_spec("hooked_mod")
print(spec.name)
print(spec.origin)
print(type(spec.loader).__name__)
import hooked_mod
print(hooked_mod.answer)
print(hooked_mod.loaded_by)
print(hooked_mod.__spec__ is spec)
print(hooked_mod.__loader__ is spec.loader)
