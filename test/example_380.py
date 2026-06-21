class Base:
    def __class_getitem__(cls, item):
        return (cls.__name__, item)

class Child(Base):
    pass

print(Base["a"])
print(Child["b"])
