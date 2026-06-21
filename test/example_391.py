import importlib.util
import pathlib
import py_compile
import sys

root = pathlib.Path("/tmp/clamp_pyc_example_391")
root.mkdir(exist_ok=True)

source = root / "byte_mod_391.py"
pyc = root / "byte_mod_391.pyc"
source.write_text("VALUE = 41\nANSWER = VALUE + 1\n")
py_compile.compile(str(source), cfile=str(pyc), doraise=True)

spec = importlib.util.spec_from_file_location("byte_mod_391", str(pyc))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
print(type(spec.loader).__name__)
print(module.ANSWER)
print(module.__cached__.endswith("byte_mod_391.pyc"))
print(module.__spec__.origin.endswith("byte_mod_391.pyc"))

import_source = root / "only_byte_391.py"
import_pyc = root / "only_byte_391.pyc"
import_source.write_text("VALUE = 99\n")
py_compile.compile(str(import_source), cfile=str(import_pyc), doraise=True)
import_source.unlink()
sys.path.insert(0, str(root))
import only_byte_391
print(only_byte_391.VALUE)
print(only_byte_391.__file__.endswith("only_byte_391.pyc"))
print(only_byte_391.__cached__.endswith("only_byte_391.pyc"))
