import_value = "outer module name"
VALUE = "outer value"
sub = "outer sub"


def local_plain_import():
    import import_value
    print(import_value.VALUE)


def local_import_alias():
    import import_pkg.sub as imported_sub
    print(imported_sub.VALUE)


def local_from_import():
    from import_value import VALUE
    print(VALUE)


def local_from_import_alias():
    from import_pkg import sub as imported_sub
    print(imported_sub.VALUE)


local_plain_import()
print(import_value)
local_import_alias()
print(sub)
local_from_import()
print(VALUE)
local_from_import_alias()
print(sub)
