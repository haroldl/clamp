import inspect


def target(left: int, right: int = 3, *, mode: str = "x", required: float) -> int:
    return right


sig = inspect.signature(target)
params = sig.parameters
print(params["left"].annotation is int, params["left"].default is inspect.Parameter.empty)
print(params["right"].annotation is int, params["right"].default)
print(params["mode"].kind == inspect.Parameter.KEYWORD_ONLY, params["mode"].default)
print(params["required"].kind == inspect.Parameter.KEYWORD_ONLY, params["required"].default is inspect.Parameter.empty)
print(sig.return_annotation is int)
