class Meta(type):
    def __new__(mcls, name, bases, namespace, marker=None):
        namespace["marker"] = marker
        return super().__new__(mcls, name, bases, namespace)

meta, ns, kw = __import__("types").prepare_class("Created", ())
print(meta.__name__)
Created = Meta("Created", (), {}, marker="ok")
print(isinstance(Created, type))
print(Created.marker)
