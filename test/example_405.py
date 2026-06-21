import importlib.resources as resources
import importlib.resources.abc as abc
from importlib.resources.abc import ResourceReader, Traversable, TraversableResources

print(resources.__name__)
print(type(resources.ResourceReader).__name__, resources.ResourceReader is abc.ResourceReader)
print(hasattr(resources, "Traversable"), hasattr(resources, "TraversableResources"))
print(abc.__name__)
print(abc.__all__)
print(ResourceReader.__name__, Traversable.__name__, TraversableResources.__name__)
print(hasattr(Traversable, "iterdir"), hasattr(Traversable, "read_bytes"), hasattr(TraversableResources, "files"))
