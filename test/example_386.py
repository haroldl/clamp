from typing import ForwardRef, _eval_type

try:
    _eval_type(ForwardRef("MissingName"), {}, {})
except NameError as exc:
    print(type(exc).__name__)
    print("caught")
