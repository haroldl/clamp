import import_pkg

files = import_pkg.__loader__.get_resource_reader(import_pkg.__name__).files()
sub = files / "sub.py"
nested = files / "nested" / "resource.tar.gz"
no_suffix = files / "README"
hidden = files / ".profile"

source = sub.with_suffix(".txt")
archive = nested.with_suffix(".zip")
removed = nested.with_suffix("")
added = no_suffix.with_suffix(".md")
hidden_changed = hidden.with_suffix(".bak")

print(type(source).__name__)
print(source.name, source.suffix, source.stem)
print(archive.name, archive.suffix, archive.stem)
print(removed.name, removed.suffix, removed.stem)
print(added.name, added.suffix, added.stem)
print(hidden_changed.name, hidden_changed.suffix, hidden_changed.stem)
print(str(source).endswith("test/import_pkg/sub.txt"))
print(str(archive).endswith("test/import_pkg/nested/resource.tar.zip"))
print(str(removed).endswith("test/import_pkg/nested/resource.tar"))
