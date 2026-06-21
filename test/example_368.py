import importlib.machinery as m
import importlib.util as u
import sys

print(sys.implementation.name)
print(sys.implementation.cache_tag)
print(u.cache_from_source("pkg/mod.py"))
print(u.source_from_cache("pkg/__pycache__/mod.cpython-312.pyc"))
print(u.cache_from_source("pkg/mod.py", optimization=""))
print(u.cache_from_source("pkg/mod.py", optimization=1))
spec = u.find_spec("import_value")
print(spec.cached.endswith("test/__pycache__/import_value.cpython-312.pyc"))
print(u.source_from_cache(spec.cached).endswith("test/import_value.py"))
file_spec = u.spec_from_file_location("direct_value", "test/import_value.py")
print(file_spec.cached)
for bad in ["pkg/mod.pyc", "pkg/__pycache__/mod.pyc", "pkg/__pycache__/mod.cpython-312.bad.pyc"]:
    try:
        u.source_from_cache(bad)
    except ValueError as exc:
        print(type(exc).__name__)
print(u.resolve_name(".sub", "import_pkg"))
source_hash = u.source_hash(b"abc")
print(type(source_hash).__name__, len(source_hash))
lazy = u.LazyLoader(m.SourceFileLoader("lazy_value", "test/import_value.py"))
print(type(lazy).__name__, lazy.loader.name, hasattr(lazy, "exec_module"))
factory = u.LazyLoader.factory(m.SourceFileLoader)
factory_loader = factory("lazy_value", "test/import_value.py")
print(type(factory_loader).__name__, factory_loader.loader.name)
print(type(u.MAGIC_NUMBER).__name__, len(u.MAGIC_NUMBER))
print(u.decode_source(b"# coding: utf-8\r\nvalue = 1\r\n").splitlines()[-1])
print(u._find_spec("import_value").name)
print(hasattr(m, "WindowsRegistryFinder"), m.WindowsRegistryFinder.find_spec("missing") is None)
print(len(m.DEBUG_BYTECODE_SUFFIXES), len(m.OPTIMIZED_BYTECODE_SUFFIXES), m.DEBUG_BYTECODE_SUFFIXES == m.BYTECODE_SUFFIXES)

