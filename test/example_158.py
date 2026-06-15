import import_pkg

print(type(import_pkg.__spec__._uninitialized_submodules).__name__)
print(len(import_pkg.__spec__._uninitialized_submodules))
import import_pkg.sub
print(len(import_pkg.__spec__._uninitialized_submodules))
print(type(import_pkg.sub.__spec__._uninitialized_submodules).__name__)
print(len(import_pkg.sub.__spec__._uninitialized_submodules))
