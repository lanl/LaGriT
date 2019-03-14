# tinerator

## loadDEM
```python
loadDEM(filepath:str, lagrit_exe:str=None)
```

Loads a DEM raster from a local filepath and returns
a `tinerator.DEM` instance.

__Attributes__

- `filepath (str)`: Filepath to DEM raster
- `lagrit_exe (str,None)`: Optional filepath to LaGriT binary. If PyLaGriT is
                       configured correctly, this should be unnecessary.

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

__Optional Arguments__

filename (str): file to write log output to

### plot
```python
DEM.plot(self, plot_out=None)
```

Draws the DEM and distance map.

__Optional Arguments__

plot_out (str): file path to save image

__Example__

```python
dem = loadDEM("example.asc")
dem.plot()

### fillDepressions
```python
DEM.fillDepressions(self, fill_depressions:bool=True, fill_flats:bool=True)
```

Fills flats and depressions on DEM. On meshes intended to be high-
resolution, leaving flats and depressions untouched may cause solver
issues. This method should be called before generating a triplane.

__Arguments__

- __fill_depressions (bool)__: fill pits and depressions on DEM
- __fill_flats (bool)__: fill flats on DEM

__Example__

```python
dem1 = loadDEM("example.asc")
dem2 = loadDEM("example.asc")

dem1.fillDepressions()

plt.imshow(dem1.dem - dem2.dem)
plt.show()

### watershedDelineation
```python
DEM.watershedDelineation(self, threshold:float=None, plot:bool=False, spacing:float=None, method:str='D8')
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
- __plot (bool)__: plot the watershed delineation and captured feature
- __spacing (float)__: the 'resolution' of the feature polygon
- __method (str)__: Flow calculation method

__Returns__

Polyline of feature as ordered (x,y) pairs

### generateBoundary
```python
DEM.generateBoundary(self, distance:float)
```

Generates a set of spaced nodes corresponding to the boundary of the DEM,
where the boundary is defined as the intersection of noDataValue and elevation data.

__Attributes__

- `distance (float)`: Euclidean distance between adjacent boundary nodes

__Returns__

vertices of boundary

### buildUniformTriplane
```python
DEM.buildUniformTriplane(self, min_edge:float, plot:bool=False, smooth_boundary:bool=False, flip:str='y', apply_elevation:bool=True)
```

Generates a triplane with uniformly sized elements.

__Attributes__

- `min_edge (float)`: triangle edge lengths

__Optional Arugments__

plot (bool): display the triangulation on completion
flip (str): flips array of the elevation raster along a given axis (x,y,xy)

### buildRefinedTriplane
```python
DEM.buildRefinedTriplane(self, min_edge:float, delta:float=0.75, outfile:str=None, slope:float=2.0, refine_dist:float=0.5, apply_elevation:bool=True, flip:str='y', plot:bool=False, smooth_boundary:bool=False)
```

Generates a refined triangular mesh, with a minimum refinement length
defined by h.

__Attributes__

- `min_edge (float)`: triangle edge lengths

__Optional Arugments__

plot (bool): display the triangulation on completion
flip (str): flips array of the elevation raster along a given axis (x,y,xy)

### layeredMesh
```python
DEM.layeredMesh(self, layers, matids=None, xy_subset=None, outfile=None)
```

Builds a layered mesh from a triplane.

__Arguments__

- __layers (list<float>)__: List of sequential layer thicknesses

__Optional Arguments__

matids (list<int): List of numbers to set as material ID for a list
outfile (str): Filepath to save mesh to
xy_subset ():


__Example__

```python
layers = [1.,1.,3.,10.,2.]
matids = [1,1,2,1,3]

dem.layeredMesh(layers,matids=matids)
```


### addAttribute
```python
DEM.addAttribute(self, data, layers=None, attribute_name=None, outfile=None, dtype=<class 'float'>)
```

Adds an attribute to the stacked mesh, over one or more layers. Default is all.
Data must be an NxM matrix - it does not necessarily have to be the same size at the DEM,
but is recommended as it will be streched to span the domain of it.

attribute_name will be the element-based attribute the data is written into.
The default is 'material ID', but can be changed to any
[a-z][A-Z][0-9] string (outside of reserved LaGriT keywords).

:param data: NxM matrix of data to be written as matrix
:type data: np.ndarray
:param layers: Layer IDs to write attributes to. Defaults to 'all'.
:type layers: list<int>
:param attribute_name: Attribute name to store data in. Defaults to material ID
:type attribute_name: str
:param outfile: Filename to write mesh to
:type outfile: str
:param dtype: Data type of elements in data. Defaults to float
:type dtype: type


### mapFunctionToAttribute
```python
DEM.mapFunctionToAttribute(self, operator='+', layers=None, attribute_name=None, outfile=None, fn=<function DEM.<lambda> at 0x1a21c7fe18>)
```


Maps a function and on operator onto mesh data.
The function fn should take one parameter: the current layer
number. The operator will perform on the data and function result.

In other words, the new attribute data will be a result of:

   attribute_data(layer) = attribute_data [operation] fn(layer)

For fn = lambda layer: layer*100 and operator '+',

   attribute_data(layer) = attribute_data + layer*100

meaning that if a selection of attribute data is

    [1,3,5,10,12...]

then, with operator '+' and lambda layer: layer*100,

    layer 1: [101,103,105,110,112...]
    layer 2: [201,203,205,210,212...]
    layer 3: [301,103,305,310,312...]
    ...


### generateFacesets
```python
DEM.generateFacesets(self, outfile, facesets=None, naive=False)
```

Generate boundary face sets according to normal vectors and layer ID.

:param lg: running instance of PyLaGriT
:type lg: pylagrit.PyLaGriT
:param outfile: filepath to save Exodus facesets
:type outfile: str
:param facesets: generated facesets integer array with length equal to boundary
:type facesets: np.ndarray
:param naive: flag to generate Exodus mesh with 3 facesets: top, bottom, sides
:type naive: bool


### calculateDistanceField
```python
DEM.calculateDistanceField(self, accumulation_threshold:float=75.0, mask:bool=True, normalize:bool=True)
```

Calculates the distance field for a DEM.
To adjust the visibility of features, you may have to
tweak the accumulation threshold.

:param accumulation_threshold: feature threshold
:type accumulation_threshold: float
:param mask: flag to mask distance map similar to DEM
:type mask: bool
:returns: generated distance field

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

__Optional Arguments__

mpl_style (bool): Change the format of the returned extent

__Returns__

DEM domain bounding box

