# tinerator.dump

## to_exodus
```python
to_exodus(dem_object, outfile:str, facesets:list=None, mesh:str=None)
```

Writes a mesh in the Exodus file format.
Note that to export with facesets, the mesh must
have depth - that is, `build_layered_mesh()` must have been
called first.

__Arguments__

- __dem_object (tinerator.DEM)__: a Tinerator DEM object
- __outfile (str)__: path to save Exodus mesh to
- __facesets (list)__: a list containing Faceset objects
- __mesh (str)__: type of mesh to export ('surface' or 'prism')

## to_avs
```python
to_avs(dem_object, outfile:str, mesh_object:str=None)
```

Writes a mesh in the AVS-UCD file format.
Note that facesets cannot be exported.

__Arguments__

- __dem_object (tinerator.DEM)__: a Tinerator DEM object
- __outfile (str)__: path to save Exodus mesh to
- __mesh (str)__: type of mesh to export ('surface' or 'prism')

