# tinerator.dem_class

## DEM
```python
DEM(self, filepath:str, lagrit_exe:str=None)
```

This is the 'main' class of TINerator, and stores all DEM and GIS data
related to a particular project.

__Attributes__

- `filepath (str)`: Filepath to DEM raster
- `lagrit_exe (str,None)`: Optional filepath to LaGriT binary. If PyLaGriT is
configured correctly, this should be unnecessary.

### change_ndv
```python
DEM.change_ndv(self, ndv:float)
```

Changes `no_data_value` of the DEM object.
This should be used instead of resetting `dem.no_data_value`
manually, as it makes deeper changes.

__Example__

```python
dem.change_ndv(-9999.)
print(dem.no_data_value) # -9999.0
```

__Arguments__

- __ndv (float)__: New `no_data_value`

### set_to_ndv
```python
DEM.set_to_ndv(self, value:float)
```

Changes all occurances of `value` in the DEM data
to `no_data_value`.

__Example__

```
dem.set_to_ndv(dem.dem[0][0])
print(dem.dem == dem.no_data_value) # True
```

__Arguments__

- __value (float)__: raster value to replace

### set_verbosity
```python
DEM.set_verbosity(self, verb_level:int, filename:str=None)
```

Set the verbosity level of printed output.

* `NOTHING` : Nothing (except warnings and related)
* `INFO` : Log output
* `FULL` : Log output and LaGriT output
* `DEBUG` : Log output, LaGriT output, and turns on debug mode

Each of these verbosity levels are variables in `tinerator.config`.

__Example__


```python
dem.set_verbosity(tinerator.config.FULL)
```

__Arguments__

- __verb_level (int)__: verbosity level
- __filename (str)__: file to write log output to

### fill_depressions
```python
DEM.fill_depressions(self, fill_depressions:bool=True, fill_flats:bool=True)
```

Fills flats and depressions on DEM. On meshes intended to be high-
resolution, leaving flats and depressions untouched may cause solver
issues. This method should be called before generating a triplane.

__Arguments__

- __fill_depressions (bool)__: fill pits and depressions on DEM
- __fill_flats (bool)__: fill flats on DEM

__Example__

```python
dem1 = tin.load.from_file("example.asc")
dem2 = tin.load.from_file("example.asc")

dem1.fill_depressions()

plt.imshow(dem1.dem - dem2.dem)
plt.show()
```

This example shows the different in topology between
a post-processed and unprocessed DEM.

### watershed_delineation
```python
DEM.watershed_delineation(self, threshold:float, method:str='D8', exponent:float=None, interactive:bool=False)
```

Performs watershed delineation on a DEM and returns a set of points
corresponding to the feature.

Available methods are:

* D8
* D4
* Rho8
* Rho4
* Dinf
* Quinn
* Holmgren
* Freeman

__Arguments__

- __threshold (float)__: threshold for determining feature from noise
- __method (str)__: Flow calculation method
- __interactive (bool)__: if True and function is called within a
                    Jupyter notebook, then function params
                    can be controlled with sliders

__Returns__

Polyline of feature as ordered (x,y) pairs

### build_uniform_triplane
```python
DEM.build_uniform_triplane(self, edge_length:float, smooth_boundary:bool=False, flip:str='y', apply_elevation:bool=True, outfile:str=None, rectangular_boundary:bool=False, boundary_distance:float=None, interactive:bool=False)
```

Generates a triplane with uniformly sized elements.

__Attributes__

- `edge_length (float)`: desired lengths for triangle edges
- `flip (str)`: flips array of the elevation raster along a given axis (x,y,xy)
- `smooth_boundary (bool)`: If True, smooth the DEM boundary for better interpolation
- `apply_elevation (bool)`: If True, interpolate DEM elevations onto surface mesh
- `outfile (str)`: filepath to save generated mesh to
- `rectangular_boundary (bool)`: set to true if the DEM domain is rectangular
- `boundary_distance (float)`: Overrides edge length and manually sets
                           spacing between boundary nodes
- `interactive (bool)`: if True and function is called within a
                    Jupyter notebook, then function params
                    can be controlled with sliders

__Returns__

PyLaGriT mesh object

### build_refined_triplane
```python
DEM.build_refined_triplane(self, min_edge_length:float, max_edge_length:float, outfile:str=None, apply_elevation:bool=True, slope:float=2.0, refine_dist:float=0.5, flip:str='y', smooth_boundary:bool=False, rectangular_boundary:bool=False, boundary_distance:float=None, interactive:bool=False)
```

Generates a refined triangular mesh, with a minimum refinement length
defined by h.

__Attributes__

- `min_edge_length (float)`: minimum triangle edge lengths
- `max_edge_length (float)`: maximum triangle edge lengths
- `outfile (str)`: Filepath to save mesh to
- `apply_elevation (bool)`: If True, interpolate DEM elevations onto surface mesh
- `slope (float)`: slope of refine function
- `refine_dist (float)`: Threshold for minimum distance in distance map
- `flip (str)`: flips array of the elevation raster along a given axis (`'x','y','xy'`)
- `smooth_boundary (bool)`: If True, smooth the DEM boundary for better interpolation
- `rectangular_boundary (bool)`: set to true if the DEM domain is rectangular
- `boundary_distance (float)`: Overrides edge length and manually sets spacing between boundary nodes
- `interactive (bool)`: if True and function is called within a
                    Jupyter notebook, then function params
                    can be controlled with sliders

__Returns__

PyLaGriT mesh object

### build_layered_mesh
```python
DEM.build_layered_mesh(self, layers, matids=None, outfile:str=None)
```

Builds a layered mesh from a triplane.

__Arguments__

- __layers (list<float>)__: List of sequential layer thicknesses
- __matids (list<int>)__: List of material IDs to set each respective layer to
- __outfile (str)__: Filepath to save mesh to

__Example__

```python
layers = [1.,1.,3.,10.,2.]
matids = [1,1,2,1,3]

dem.build_layered_mesh(layers,matids=matids)
```


### add_attribute
```python
DEM.add_attribute(self, data, layers=None, attribute_name=None, outfile=None, dtype=None)
```

Adds an attribute to the stacked mesh, over one or more layers. Default is all.
Data must be an NxM matrix - it does not necessarily have to be the same size at the DEM,
but is recommended as it will be streched to span the domain of it.

`attribute_name` will be the element-based attribute the data is written into.
The default is 'material ID' (`itetclr`), but can be changed to any
[a-z][A-Z][0-9] string (outside of reserved LaGriT keywords).

__Arguments__

- __data (np.ndarray)__: NxM matrix of data to be written as matrix
- __layers (list<int>)__: Layer IDs to write attributes to. Defaults to 'all'.
- __attribute_name (str)__: Attribute name to store data in. Defaults to material ID
- __outfile (str)__: Filename to write mesh to
- __dtype (type)__: Data type of elements in data (`float` or `int`)

### map_function_to_attribute
```python
DEM.map_function_to_attribute(self, operator='+', layers=None, attribute_name=None, outfile=None, fn=<function DEM.<lambda> at 0x1c337f2400>)
```


Maps a function and on operator onto mesh data.
The function fn should take one parameter: the current layer
number. The operator will perform on the data and function result.

In other words, the new attribute data will be a result of:

     attribute_data(layer) = attribute_data [operation] fn(layer)

For `fn = lambda layer: layer*100` and operator `+`,

     attribute_data(layer) = attribute_data + layer*100

meaning that if a selection of attribute data is

     [1,3,5,10,12...]

then, with operator '+' and lambda layer: layer*100,

      layer 1: [101,103,105,110,112...]
      layer 2: [201,203,205,210,212...]
      layer 3: [301,103,305,310,312...]
      ...


### getBoundingBox
```python
DEM.getBoundingBox(self, mpl_style:bool=True)
```

Returns the bounding box (or extent) of the DEM domain.

By default, the format of the extent returned is:

    (x_min,x_max,y_min,y_max)

By setting `mpl_style=False`, the format changes to:

    (x_min,y_min,x_max,y_max)

Extent units are relative to the parent DEM coordinate system.

__Arguments__

- __mpl_style (bool)__: Change the format of the returned extent

__Returns__

DEM domain bounding box

### plot_dem
```python
DEM.plot_dem(self, hillshade:bool=False, plot_out:str=None)
```

Plots the loaded DEM.

### plot_boundary
```python
DEM.plot_boundary(self)
```

Plots the DEM domain boundary (if available).

### plot_feature
```python
DEM.plot_feature(self)
```

Displays the feature captured by performing watershed delination
(if available).

