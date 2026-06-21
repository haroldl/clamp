def show_type(type):
    print(type)
    print(type is __import__("builtins").type)


def show_list(list):
    print(list)
    print(list is __import__("builtins").list)


show_type("value")
show_list("items")
