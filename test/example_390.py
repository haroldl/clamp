dict = "module dict"


class Demo:
    dict = "class dict"

    def read_dict(self):
        print(dict)

    def dict(self):
        return "method dict"


Demo().read_dict()
print(Demo().dict())
