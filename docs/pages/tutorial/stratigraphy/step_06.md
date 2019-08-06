## 6. Truncate with Polyline

We now have a mesh with complex stratigraphy encoded in its material ID. 
However, the domain of this mesh is a rather boring cuboid and doesn't
accurately reflect the geospatial domain that we are trying to model.

By importing a polyline, the exterior boundary of the mesh can be truncated.
As in previous steps, we will use a mesh to define a `region` that will be used
for element-wise operations.

However, the boundary is a line object. In order to use it as a surface/region,
it must be a surface. A polyline can be turned into a vertical surface by
'extruding' it in the vertical (0,0,1) direction:

```
read / avs / basin_bnd_ply_rescale.inp / mo_bndry
extrude / mo_fence / mo_bndry / const / 3200. / volume / 0. 0. -1.
```

We will also translate the extrusion so that it covers the vertical extent
of the hex mesh:

```
trans / 1 0 0 / 0. 0. -3200. / 0. 0. 0.
```

Next, we use the boundary to truncate (remove) cells outside the boundary.

There are three ways to define 'outside':

1. Only remove a cell if ALL vertices are outside
2. Remove a cell if the centroid (average of all vertices) is outside
3. Remove a cell if one or more vertices are outside

```
cmo / select / MONAME
surface / s_bndry / reflect / sheet / mo_fence
cmo / select / MONAME
region / r_bndry / ge s_bndry
pset / p_bndry / region r_bndry
```
**Method 1:**

    eltset / e_delete1 / exclusive / pset get p_bndry

**Method 2:**

    eltset / e_delete2 / region r_bndry

**Method 3:**

    eltset / e_delete3 / inclusive / pset get p_bndry