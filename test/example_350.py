import _json

module_dict = vars(_json)
module_dict["__clamp_dict_probe__"] = "live"
print(_json.__clamp_dict_probe__)
print("__clamp_dict_probe__" in dir(_json))
del module_dict["__clamp_dict_probe__"]
print(hasattr(_json, "__clamp_dict_probe__"))
_json.__clamp_dict_probe2__ = "attr"
module_dict = vars(_json)
print(module_dict["__clamp_dict_probe2__"])
del _json.__clamp_dict_probe2__
print("__clamp_dict_probe2__" in module_dict)
