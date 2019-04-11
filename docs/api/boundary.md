# tinerator.boundary

## orderPointsClockwise
```python
orderPointsClockwise(points:numpy.ndarray, opt:str='polar', clockwise:bool=True)
```

Given a 2D array of points, this function reorders points clockwise.
Available methods are: 'angle', to sort by angle, 'polar', to sort by
polar coordinates, and 'nearest_neighbor', to sort by nearest neighbor.

__Arguments__

- __points (np.ndarray)__: Array of unsorted points
- __opt (str)__: Sorting method
- __clockwise (bool)__: order points clockwise or counterclockwise

__Returns__

Sorted points

## imageErosionBoundary
```python
imageErosionBoundary(A, nil_value, distance, cell_size=None, xll_corner=0, yll_corner=0)
```

Blazing fast way to create an accurate DEM boundary.
Currently, there is no way to seperate nodes with a delta-x.
As such, this function is not called anywhere within this package.
Once that is implemented, this function will depreciate squareTraceBoundary().

__Arguments__

- __A (np.ndarray)__: matrix to perform boundary analysis on
- __nil_value (float)__: value characterizing undefined array values
- __distance (float)__: spacing between boundary nodes

__Returns__

boundary nodes

## rectangularBoundary
```python
rectangularBoundary(bbox:list, spacing:float)
```

Generates a rectangular boundary with evenly spaced points.

bbox should be a list of values in the following format:

    min(x), max(x), min(y), max(y)

__Arguments__

- __bbox (list<float>)__: bounding box coordinates
- __spacing (float)__: spacing between adjacent nodes

__Returns__

Array of interpolated bounding box values

## squareTraceBoundary
```python
squareTraceBoundary(A, NDV, dist=10.0)
```

Uses a square-tracing algorithm to quickly find a set of points
composing the boundary at the interface of data and "empty" (noDataValue)
cells in a DEM matrix.

The optional parameter 'dist' denotes the seperation the points should
have between each other.
A smaller value will result in more points being created.

For more information, visit [this page](http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/square.html).

__Arguments__

- __A (np.ndarray)__: matrix to perform boundary analysis on
- __NDV (float)__: value characterizing undefined array values
- __dist (float)__: spacing between boundary nodes

__Returns__

Boundary nodes

