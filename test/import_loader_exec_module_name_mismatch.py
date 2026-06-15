import import_exec_module_target

import_exec_module_target.__name__ = "renamed_exec_module_target"
import_exec_module_target.__loader__.exec_module(import_exec_module_target)
print("should not execute")
