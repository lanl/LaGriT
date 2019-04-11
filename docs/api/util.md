# tinerator.utilities


Set of functions that convert from matrix (row and column) units to
a DEM projection, and vice versa.


## filter_points
```python
filter_points(points:numpy.ndarray, eps:float)
```

Removes points that are within `eps` distance of each other.

__Arguments__

- __points (np.ndarray)__: point array to filter
- __eps (float)__: remove adjacent points within this distance of each other

__Returns__

Filtered points

