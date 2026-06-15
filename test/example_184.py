import import_exec_module_target

loader = import_exec_module_target.__loader__
print(import_exec_module_target.VALUE)
import_exec_module_target.VALUE = "mutated"
print(import_exec_module_target.VALUE)
print(loader.exec_module(import_exec_module_target))
print(import_exec_module_target.VALUE)
print(import_exec_module_target.__loader__ is loader)
print(import_exec_module_target.__spec__._initializing)
