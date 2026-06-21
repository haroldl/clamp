import importlib.machinery
import importlib.util
import pathlib
import sys
import zipfile

root = pathlib.Path("/tmp/clamp_zip_example_392")
root.mkdir(exist_ok=True)
archive = root / "mods.zip"
with zipfile.ZipFile(str(archive), "w") as z:
    z.writestr("zip_mod_392.py", "VALUE = 42\n")
    z.writestr("zip_pkg_392/__init__.py", "NAME = 'pkg'\n")
    z.writestr("zip_pkg_392/sub.py", "ANSWER = 99\n")

sys.path.insert(0, str(archive))
import zip_mod_392
from zip_pkg_392 import sub

print(zip_mod_392.VALUE)
print(zip_mod_392.__loader__.__class__.__name__)
print(zip_mod_392.__spec__.origin.endswith("mods.zip/zip_mod_392.py"))
print(zip_mod_392.__cached__.endswith("mods.zip/__pycache__/zip_mod_392.cpython-312.pyc"))
print(sub.ANSWER)
print(sub.__package__)
print(importlib.util.find_spec("zip_mod_392").loader.__class__.__name__)
print(importlib.machinery.PathFinder.find_spec("zip_mod_392", [str(archive)]).origin.endswith("mods.zip/zip_mod_392.py"))
