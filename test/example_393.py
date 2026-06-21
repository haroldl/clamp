import _collections
import gc

box = _collections.defaultdict(list)
box["items"].append(object())
copy = box["items"].copy()
del box["items"][0]
gc.collect()
item = copy[0]
print(item.__class__.__name__)
print(item is item)
print(bool(item))
