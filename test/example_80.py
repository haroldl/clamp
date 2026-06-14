def local_function():
    return 1

print(callable(local_function))
print(callable(print), callable(len), callable(callable))
print(callable(0), callable(True), callable(None))
print(callable([1, 2]), callable((1, 2)), callable("abc"))
method_result = [1].append(2)
print(callable(method_result))
