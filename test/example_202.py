import math

print(math.isclose(math.inf, math.inf))
print(math.isclose(math.inf, -math.inf))
print(math.isclose(math.nan, math.nan))
print(math.isfinite(math.nan))
print(math.isinf(-math.inf))
print(math.isnan(math.nan))
print(math.nextafter(1.0, 2.0) > 1.0)
print(math.ulp(1.0) == math.nextafter(1.0, 2.0) - 1.0)
