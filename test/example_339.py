import sys
print("test/path_extra" in sys.path)
sys.path.append("test/path_extra")
import path_added
print(path_added.VALUE)
