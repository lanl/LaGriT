# tinerator.load

## from_file
```python
from_file(filepath:str, lagrit_exe=None)
```

Loads a DEM raster from a local filepath and returns
a `tinerator.DEM` instance.

__Arguments__

- __filepath (str)__: Filepath to DEM raster

## from_matrix
```python
from_matrix(array:numpy.ndarray, xllcorner=0.0, yllcorner=0.0, cell_size=10.0, no_data_value=-9999)
```

Creates a TINerator DEM instance from a Numpy 2D matrix.

__Arguments__

- __array (np.ndarray)__: NxM Numpy matrix
- __* xllcorner (int,float)__: lower-left raster x-coordinate
- __* yllcorner (int,float)__: lower-left raster y-coordinate
- __* cell_size (int,float)__: raster cell size
- __* no_data_value (int,float)__: raster null value

- __**Note__: no_data_value cannot be np.nan.**

__Returns__

`tinerator.DEM` instance

## from_coordinates
```python
from_coordinates(bounding_box, outfile=None, SRTM_30=True)
```

Downloads and returns a DEM from a geodetic coordinate bounding box.

This function is powered by the [elevation](https://pypi.org/project/elevation/)
package, and sources rasters from either SRTM 30m Global 1 arc second V003
elaborated by NASA and NGA,
or SRTM 90m Digital Elevation Database v4.1 elaborated by CGIAR-CSI.

Note that note bounding box values must be latitude and longitude values
given as: `(left, bottom, right, top)`.

(Specifically, geodetic coordinates in the WGS84 reference system EPSG:4326.)

Note: if this method fails with error 'Too many tiles', try again with
`SRTM_30=False`.

__Arguments__

- __bounding_box (tuple)__: Lat/long bounding box values
- __outfile (str)__: File path to save downloaded raster
- __SRTM_30 (bool)__: If True, use SRTM 30m. If False, SRTM 90m.

__Returns__

`tinerator.DEM` instance

## from_shapefile
```python
from_shapefile(shapefile, outfile=None, SRTM_30=True)
```

Downloads and returns the DEM enclosed by a shapefile.

This function is powered by the [elevation](https://pypi.org/project/elevation/)
package, and sources
rasters from either SRTM 30m Global 1 arc second V003 elaborated by
NASA and NGA, or SRTM 90m Digital Elevation Database v4.1 elaborated
by CGIAR-CSI.

Note: if this method fails with error 'Too many tiles', try again with
`SRTM_30=False`.

__Arguments__

- __shapefile (str)__: Path to ESRI Shapefile
- __outfile (str)__: File path to save downloaded raster
- __SRTM_30 (bool)__: If True, use SRTM 30m. If False, SRTM 90m.

__Returns__

`tinerator.DEM` instance

