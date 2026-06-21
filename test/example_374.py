import inspect

class Widget:
    def validate(self):
        return self

    @classmethod
    def normalize(cls, value):
        return value

print(list(inspect.signature(Widget.validate).parameters))
print(list(inspect.signature(Widget().validate).parameters))
print(list(inspect.signature(Widget.normalize).parameters))
