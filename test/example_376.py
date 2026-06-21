def marker():
    return "module"

class C:
    marker = "class"

    @classmethod
    def check(cls):
        return marker()

print(C.check())
