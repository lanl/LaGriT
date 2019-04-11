# tinerator.facesets

## Faceset
```python
Faceset(self, fs_type, data, metadata=None)
```

Object that stores faceset formatting data

## from_elevations
```python
from_elevations(heights:list, keep_body:bool=False) -> tinerator.facesets.Faceset
```

Facesets are generated and return from the heights array:
a faceset will be created in the layers defined by `heights`.

For example, with the `heights` array

```python
heights = [50.,100.,150.,200.]
```

five facesets will be created:

* all surface elements lower than 50 meters
* all surface elements between 50 and 100 meters
* all surface elements between 100 and 150 meters
* all surface elements between 150 and 200 meters
* all surface elements greater than 200 meters

Another approach is to split the elevation range into layers using
properties `min_z` and `max_z`:

```python
>>> print('Elevation range = ({0},{1})'.format(dem.min_z,dem.max_z))
Elevation range = (2365.3,3942.2)

>>> heights = np.linspace(dem.min_z,dem.max_z,10)
>>> print(heights)
array([2365.3       , 2540.51111111, 2715.72222222, 2890.93333333,
       3066.14444444, 3241.35555556, 3416.56666667, 3591.77777778,
       3766.98888889, 3942.2       ])

```

__Arguments__

- __dem_object (tinerator.DEM)__: an instance of tinerator.DEM class
- __heights (list<float>)__: a list of vertical (z) layers
- __keep_body (bool)__: when True, elevation-based facesets are applied across
the *entire mesh*. When False, elevation-based facesets only apply to
the top layer.

__Returns__

A Faceset object

## sidesets
```python
sidesets(coords:numpy.ndarray, top_layer:bool=False) -> tinerator.facesets.Faceset
```

Operates on side facesets *only*.

Constructs discretized side facesets based on the coords array.
`coords` should contain one [x,y] pairs at each point a new sideset
should be defined. Further, these points must be ordered clockwise.

For an example, consider a square that spans 0 to 1 in both the
x and y planes. The top, right, and bottom facesets are represented
in the drawing below:

```
    1
 _______
|       |
|       | 2
|       |
 -------
    3

```

To construct `coords` properly, the array would look like:

```
[0.,1.], # top
[1.,1.], # right
[1.,0.]  # bottom
```

Note the points are ordered clock-wise.

------------------------

By default, these sidesets will be applied to all layers.
We can apply these sidesets to only the top layer (to capture an
outlet, for example) by using flag `top_layer=True`.

__Arguments__

- __coords (np.ndarray)__: clockwise array of points indicating faceset junctions
- __top_layer (bool)__: when True, apply to only the top layer. when False, apply
to all layers.

__Returns__

A Faceset class instance

## basic
```python
basic(has_top:bool=True, has_sides:bool=True, has_bottom:bool=True) -> tinerator.facesets.Faceset
```

Generates basic facesets. Using the flags, you can define one or
more of:

* Top faceset
* Side faceset
* Bottom faceset

__Arguments__

- __has_top (bool)__: generate a top faceset
- __has_sides (bool)__: generate a sides faceset
- __has_bottom (bool)__: generate a bottom faceset

__Returns__

A Faceset class instance

## write_facesets
```python
write_facesets(dem_object, facesets)
```

Given a DEM object and a list of Faceset class objects,
this generates the corresponding faceset files.

This function should not be used by an end-user unless they want
AVS2 faceset files unconnected to an Exodus mesh.

__Arguments__

- __dem_object (tinerator.DEM)__: DEM class instance to operate from
- __facesets (list<tinerator.Faceset>)__: list of Faceset objects describing
generation steps

__Returns__

A list containing generated faceset files

